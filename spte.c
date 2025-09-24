/** libraries **/

#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

/** defines **/

#define CTRL_KEY(k) ((k) & 0x1f)
#define SPTE_VERSION "1.0.0"
#define TAB_STOP 4
#define QUIT_CONFIRMATION 3

enum editorKey{
    BACKSPACE = 127,
    ARROW_LEFT = 1000,
    ARROW_RIGHT,
    ARROW_UP,
    ARROW_DOWN,
    DEL_KEY,
    HOME_KEY,
    END_KEY,
    PAGE_UP,
    PAGE_DOWN
};

enum highlight{
    HL_NORMAL = 0,
    HL_COMMENT,
    HL_MLCOMMENT,
    HL_KEYWORD1,
    HL_KEYWORD2,
    HL_STRING,
    HL_NUMBER,
    HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1<<0)
#define HL_HIGHLIGHT_STRING (1<<1)

/** data **/

struct editorSyntax{
    char *filetype;
    char **filematch;
    char **keywords;
    char *singleLineStart;
    char *multiLineStart;
    char *multiLineEnd;
    int flags;
};

typedef struct erow{
    int idx;
    int size;
    int rsize;
    char *chars;
    char *render;
    unsigned char *hl;
    int hlOpenComment;
} erow;

struct editorConfig{
    int cx, cy;
    int rx;
    int rowOff;
    int colOff;
    int screenrows;
    int screencols;
    int rawScreenrows;
    int rawScreencols;
    int numRows;
    char *filename;
    erow *row;
    int dirty;
    int linenumIndent;
    char statusmsg[80];
    time_t statusmsg_time;
    struct editorSyntax *syntax;
    struct termios origTermios;
};

struct editorConfig E;

/** filetypes **/

char *C_HL_extensions[] = {".c", ".h", ".cpp", NULL };
char *C_HL_keywords[] = {"switch", "if", "while", "for", "break", "continue", "return", "else", "struct", "union", "typedef", "static", "enum", "class", "case",
                        "int|", "long|", "double|", "float|", "char|", "unsigned|", "signed|", "void|", NULL };

struct editorSyntax HLDB[] = {
    {
        "c",
        C_HL_extensions,
        C_HL_keywords,
        "//", "/*", "*/",
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRING
    },
};

#define HLDB_ENTRIES (sizeof(HLDB) / sizeof(HLDB[0]))

/** function prototypes **/

void setStatusMessage(const char *fmt, ...);
void refreshScreen();
char *editorPrompt(char *prompt, void(*callback)(char*, int));

/** terminal **/

void kill(const char *s){
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);

    perror(s);
    exit(1);
}

void disableRaw(){
    if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.origTermios) == -1) kill("tcsetattr");
}

void enableRaw(){
    if(tcgetattr(STDIN_FILENO, &E.origTermios) == -1) kill("tcgetattr");
    atexit(disableRaw);

    struct termios raw = E.origTermios;
    raw.c_iflag &= ~(BRKINT|ICRNL|INPCK|ISTRIP|IXON);
    raw.c_oflag &= ~(OPOST);
    raw.c_cflag |= (CS8);
    raw.c_lflag &= ~(ECHO|ICANON|IEXTEN|ISIG);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 1;

    if(tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1) kill("tcsetattr");
}

int readKey(){
    int nread;
    char c;
    while ((nread = read(STDIN_FILENO, &c, 1)) != 1){
        if(nread == -1 && errno != EAGAIN) kill("read");
    }

    if(c == '\x1b'){
        char seq[3];

        if(read(STDIN_FILENO, &seq[0], 1) != 1) return '\x1b';
        if(read(STDIN_FILENO, &seq[1], 1) != 1) return '\x1b';

        if(seq[0] == '['){
            if(seq[1] >= '0' && seq[1] <= '9'){
                if(read(STDIN_FILENO, &seq[2], 1) != 1) return '\x1b';
                if(seq[2] == '~'){
                    switch (seq[1]){
                        case '1': return HOME_KEY;
                        case '3': return DEL_KEY;
                        case '4': return END_KEY;
                        case '5': return PAGE_UP;
                        case '6': return PAGE_DOWN;
                        case '7': return HOME_KEY;
                        case '8': return END_KEY;
                    }
                }
            }else{
                switch (seq[1]){
                    case 'A': return ARROW_UP;
                    case 'B': return ARROW_DOWN;
                    case 'C': return ARROW_RIGHT;
                    case 'D': return ARROW_LEFT;
                    case 'H': return HOME_KEY;
                    case 'F': return END_KEY;
                }
            }
        }else if(seq[0] == 'O'){
            switch (seq[1]){
                case 'H': return HOME_KEY;
                case 'F': return END_KEY;
            }
        }
        return '\x1b';
    }else{
        return c;
    }
}

int getCursorPosition(int *rows, int *cols){
    char buf[32];
    unsigned int i = 0;

    if(write(STDOUT_FILENO, "\x1b[6n", 4) != 4) return -1;

    while (i < sizeof(buf) - 1){
        if(read(STDIN_FILENO, &buf[i], 1) != 1) break;
        if(buf[i] == 'R') break;
        i++;
    }
    buf[i] = '\0';

    if(buf[0] != '\x1b' || buf[1] != '[') return -1;
    if(sscanf(&buf[2], "%d;%d", rows, cols) != 2) return -1;

    return 0;
}

int getWinSize(int *rows, int *cols){
    struct winsize ws;
    
    if(ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0){
        if(write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12) return -1;
        return getCursorPosition(rows, cols);
    }else{
        *rows = ws.ws_row;
        *cols = ws.ws_col;
        return 0;
    }
}

/** syntax highlighting **/

int isSeparator(int c){
    return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];", c) != NULL;
}

void updateSyntax(erow *row){
    row->hl = realloc(row->hl, row->rsize);
    memset(row->hl, HL_NORMAL, row->rsize);

    if(E.syntax == NULL) return;

    char **keywords = E.syntax->keywords;

    char *scs = E.syntax->singleLineStart;
    char *mcs = E.syntax->multiLineStart;
    char *mce = E.syntax->multiLineEnd;

    int scsLen = scs ? strlen(scs) : 0;
    int mcsLen = scs ? strlen(mcs) : 0;
    int mceLen = scs ? strlen(mce) : 0;

    int prevSep = 1;
    int inString = 0;
    int inComment = (row->idx > 0 && E.row[row->idx - 1].hlOpenComment);

    int i = 0;
    while(i < row->rsize){
        char c = row->render[i];
        unsigned char prevHl = (i > 0) ? row->hl[i - 1] : HL_NORMAL;

        if(scsLen && !inString && !inComment){
            if(!strncmp(&row->render[i], scs, scsLen)){
                memset(&row->hl[i], HL_COMMENT, row->size - i);
                break;
            }
        }

        if(mcsLen && mceLen && !inString){
            if(inComment){
                row->hl[i] = HL_MLCOMMENT;
                if(!strncmp(&row->render[i], mce, mceLen)){
                    memset(&row->hl[i], HL_MLCOMMENT, mceLen);
                    i += mceLen;
                    inComment = 0;
                    prevSep = 1;
                    continue;
                }else{
                    i++;
                    continue;
                }
            }else if(!strncmp(&row->render[i], mcs, mcsLen)){
                memset(&row->hl[i], HL_MLCOMMENT, mcsLen);
                i += mcsLen;
                inComment = 1;
                continue;
            }
        }

        if(E.syntax->flags & HL_HIGHLIGHT_STRING){
            if (inString) {
                row->hl[i] = HL_STRING;
                if(c == '\\' && i + 1 < row->size){
                    row->hl[i + 1] = HL_STRING;
                    i += 2;
                    continue;
                }
                if(c == inString) inString = 0;
                i++;
                prevSep = 1;
                continue;
            }else{
                if(c == '"' || c == '\''){
                    inString = c;
                    row->hl[i] = HL_STRING;
                    i++;
                    continue;
                }
            }
        }

        if(E.syntax->flags & HL_HIGHLIGHT_NUMBERS){
            if ((isdigit(c) && (prevSep || prevHl == HL_NUMBER)) || (c == '.' && prevHl == HL_NUMBER)) {
                row->hl[i] = HL_NUMBER;
                i++;
                prevSep = 0;
                continue;
            }
        }

        if(prevSep){
            int j;
            for(j = 0; keywords[j]; j++){
                int klen = strlen(keywords[j]);
                int kw2 = keywords[j][klen - 1] == '|';
                if(kw2) klen--;

                if(!strncmp(&row->render[i], keywords[j], klen) && isSeparator(row->render[i + klen])){
                    memset(&row->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
                    i += klen;
                    break;
                }
            }
            if(keywords[j] != NULL){
                prevSep = 0;
                continue;
            }
        }

        prevSep = isSeparator(c);
        i++;
    }

    int changed = (row->hlOpenComment != inComment);
    row->hlOpenComment = inComment;
    if(changed && row->idx + 1 < E.numRows) updateSyntax(&E.row[row->idx + 1]);
}

int syntaxToColour(int hl){
    switch (hl){
        case HL_MLCOMMENT:
        case HL_COMMENT: return 36;
        case HL_KEYWORD1: return 33;
        case HL_KEYWORD2: return 32;
        case HL_STRING: return 35;
        case HL_NUMBER: return 31;
        case HL_MATCH: return 34;  
        default: return 37;
    }
}

void selectSyntaxHighlight(){
    E.syntax = NULL;
    if(E.filename == NULL) return;

    char *ext = strrchr(E.filename, '.');

    for(unsigned int j = 0; j < HLDB_ENTRIES; j++){
        struct editorSyntax *s = &HLDB[j];
        unsigned int i = 0;
        while(s->filematch[i]){
            int isExt = (s->filematch[i][0] == '.');
            if((isExt && ext && !strcmp(ext, s->filematch[i])) || (!isExt && strstr(E.filename, s->filematch[i]))){
                E.syntax = s;

                int filerow;
                for(filerow = 0; filerow < E.numRows; filerow++){
                    updateSyntax(&E.row[filerow]);
                }
                return;
            }
            i++;
        }
    }
}

/** row operations **/

int cxToRx(erow *row, int cx){
    int rx = 0;
    int j;
    for(j=0; j<cx; j++){
        if(row->chars[j] == '\t') rx += (TAB_STOP - 1) - (rx % TAB_STOP);
        rx++;
    }
    return rx;
}

int rxToCx(erow *row, int rx){
    int curRx = 0;
    int cx;
    for(cx = 0; cx < row->size; cx++){
        if(row->chars[cx] == '\t') curRx += (TAB_STOP - 1) - (curRx % TAB_STOP);
        curRx++;

        if(curRx > rx) return cx;
    }
    return cx;
}

void updateRow(erow *row){
    int tabs = 0;
    int j;
    for(j = 0; j<row->size; j++) if(row->chars[j] == '\t') tabs++;

    free(row->render);
    row->render = malloc(row->size + tabs*(TAB_STOP - 1) + 1);
    
    int idx = 0;
    for(j = 0; j<row->size; j++){
        if(row->chars[j] == '\t'){
            row->render[idx++] = ' ';
            while (idx % TAB_STOP != 0) row->render[idx++] = ' ';
        }else{
            row->render[idx++] = row->chars[j];
        }
    }
    row->render[idx] = '\0';
    row->rsize = idx;

    updateSyntax(row);

}

void insertRow(int at, char *s, size_t len){
    if(at < 0 || at > E.numRows) return;

    E.row = realloc(E.row, sizeof(erow) * (E.numRows + 1));
    memmove(&E.row[at + 1], &E.row[at], sizeof(erow) * (E.numRows - at));
    for(int j = at + 1; j <= E.numRows; j++) E.row[j].idx++;

    E.row[at].idx = at;

    E.row[at].size = len;
    E.row[at].chars = malloc(len + 1);
    memcpy(E.row[at].chars, s, len);
    E.row[at].chars[len] = '\0';

    E.row[at].rsize = 0;
    E.row[at].render = NULL;
    E.row[at].hl = NULL;
    E.row[at].hlOpenComment = 0;
    updateRow(&E.row[at]);

    E.numRows++;
    E.dirty++;
}

void freeRow(erow *row){
    free(row->render);
    free(row->chars);
    free(row->hl);
}

void delRow(int at){
    if(at < 0 || at >= E.numRows) return;
    freeRow(&E.row[at]);
    memmove(&E.row[at], &E.row[at + 1], sizeof(erow) * (E.numRows - at - 1));
    for(int j = at; j < E.numRows - 1; j++) E.row[j].idx--;
    E.numRows--;
    E.dirty++;

}

void rowInsertChar(erow *row, int at, int c){
    if(at < 0 || at > row->size) at = row->size;
    row->chars = realloc(row->chars, row->size + 2);
    memmove(&row->chars[at + 1], &row->chars[at], row->size - at + 1);
    row->size++;
    row->chars[at] = c;
    updateRow(row);
    E.dirty++;
}

void insertNewLine(){
    if(E.cx == 0){
        insertRow(E.cy, "", 0);
    }else{
        erow *row = &E.row[E.cy];
        insertRow(E.cy + 1, &row->chars[E.cx], row->size - E.cx);
        row = &E.row[E.cy];
        row->size = E.cx;
        row->chars[row->size] = '\0';
        updateRow(row);
    }
    E.cy++;
    E.cx = 0;
}

void rowAppendString(erow *row, char *s, size_t len){
    row->chars = realloc(row->chars, row->size + len + 1);
    memcpy(&row->chars[row->size], s, len);
    row->size += len;
    row->chars[row->size] = '\0';
    updateRow(row);
    E.dirty++;
}

void rowDelChar(erow *row, int at){
    if(at < 0 || at > row->size) return;
    memmove(&row->chars[at], &row->chars[at + 1], row->size - at);
    row->size--;
    updateRow(row);
    E.dirty++;
}

/** editor operations **/

void insertChar(int c){
    if(E.cy == E.numRows){
        insertRow(E.numRows, "", 0);
    }
    rowInsertChar(&E.row[E.cy], E.cx, c);
    E.cx++;
}

void delChar(){
    if(E.cy == E.numRows) return;
    if(E.cx == 0 && E.cy == 0) return;
    erow *row = &E.row[E.cy];
    if(E.cx > 0){
        rowDelChar(row, E.cx - 1);
        E.cx--;
    }else{
        E.cx = E.row[E.cy - 1].size;
        rowAppendString(&E.row[E.cy - 1], row->chars, row->size);
        delRow(E.cy);
        E.cy--;
    }
}

/** file i/o **/

char *rowsToString(int *buflen){
    int totlen = 0;
    int j;
    for(j = 0; j < E.numRows; j++) totlen += E.row[j].size + 1;
    *buflen = totlen;

    char *buf = malloc(totlen);
    char *p = buf;
    for(j = 0; j < E.numRows; j++){
        memcpy(p, E.row[j].chars, E.row[j].size);
        p += E.row[j].size;
        *p = '\n';
        p++;
    }

    return buf;
}

void openEditor(char *filename){
    free(E.filename);
    E.filename = strdup(filename);

    selectSyntaxHighlight();

    FILE *fp = fopen(filename, "r");
    if (!fp) kill("fopen");

    char *line = NULL;
    size_t lineCap = 0;
    ssize_t lineLen;
    while((lineLen = getline(&line, &lineCap, fp)) != -1){
        while(lineLen > 0 && (line[lineLen - 1] == '\n' || line[lineLen - 1] == '\r')) lineLen--;
        insertRow(E.numRows, line, lineLen);
    }
    free(line);
    fclose(fp);
    E.dirty = 0;
}

void saveEditor(){
    if(E.filename == NULL){
        E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
        if(E.filename == NULL){
            setStatusMessage("Save Aborted");
            return;
        }
        selectSyntaxHighlight();
    }

    int len;
    char *buf = rowsToString(&len);

    int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
    if(fd != -1){
        if(ftruncate(fd, len) != -1){
            if(write(fd, buf, len) == len){
                close(fd);
                free(buf);
                setStatusMessage("%d Bytes written to disk", len);
                E.dirty = 0;
                return;
            }
        }
        close(fd);
    }
    free(buf);
    setStatusMessage("Write Failed! I/O Error: %s", strerror(errno));
}

/** search **/

void searchEditorCallback(char *query, int key){
    static int lastMatch = -1;
    static int direction = 1;

    static int savedLine;
    static char *savedHl = NULL;

    if(savedHl){
        memcpy(E.row[savedLine].hl, savedHl, E.row[savedLine].rsize);
        free(savedHl);
        savedHl = NULL;
    }

    if(key == '\r' || key == '\x1b'){
        lastMatch = -1;
        direction = 1;
        return;
    }else if(key == ARROW_RIGHT || key == ARROW_DOWN){
        direction  = 1;
    }else if(key == ARROW_LEFT || key == ARROW_UP){
        direction = -1;
    }else{
        lastMatch = -1;
        direction = 1;
    }

    if(lastMatch == -1) direction = 1;
    int current = lastMatch;


    int i;
    for(i = 0; i < E.numRows; i++){
        current += direction;
        if(current == -1) current = E.numRows - 1;
        else if(current == E.numRows) current = 0;

        erow *row = &E.row[current];
        char *match = strstr(row->render, query);
        if(match){
            lastMatch = current;
            E.cy = current;
            E.cx = rxToCx(row, match - row->render);
            E.rowOff = E.numRows;

            savedLine = current;
            savedHl = malloc(row->size);
            memcpy(savedHl, row->hl, row->rsize);
            memset(&row->hl[match - row->render], HL_MATCH, strlen(query));
            break;
        }
    }
}

void searchEditor(){
    int savedCx = E.cx;
    int savedCy = E.cy;
    int savedColOff = E.colOff;
    int savedRowOff = E.rowOff;

    char *query = editorPrompt("Search: %s (Use ESC/Arrows/Enter)", searchEditorCallback);

    if(query){
        free(query);
    }else{
        E.cx = savedCx;
        E.cy = savedCy;
        E.colOff = savedColOff;
        E.rowOff = savedRowOff;
    }
}

/** append buffer **/

struct abuf{
    char *b;
    int len;
};

#define ABUF_INIT {NULL, 0}

void abAppend(struct abuf *ab, const char *s, int len){
    char *new = realloc(ab->b, ab->len+len);

    if(new == NULL) return;
    memcpy(&new[ab->len], s, len);
    ab->b = new;
    ab->len += len;
}

void abFree(struct abuf *ab){
    free(ab->b);
}

/** output **/

void editorScroll(){
    E.rx = 0;
    if(E.cy < E.numRows){
        E.rx = cxToRx(&E.row[E.cy], E.cx);
    }

    if(E.cy < E.rowOff){
        E.rowOff = E.cy;
    }
    if(E.cy >= E.rowOff + E.screenrows){
        E.rowOff = E.cy - E.screenrows + 1;
    }
    if(E.rx < E.colOff){
        E.colOff = E.rx;
    }
    if(E.rx >= E.colOff + E.screencols){
        E.colOff = E.rx - E.screencols + 1;
    }
}

void drawRows(struct abuf *ab){
    int y;
    for(y=0; y<E.screenrows; y++){
        int filerow = y + E.rowOff;

        char format[8];
        char linenum[E.linenumIndent + 1];
        memset(linenum, ' ', E.linenumIndent);
        snprintf(format, 5, "%%%dd ", E.linenumIndent - 1);
        if(filerow < E.numRows){
            snprintf(linenum, E.linenumIndent + 1, format, filerow + 1);
        }
        abAppend(ab, linenum, E.linenumIndent);

        if(filerow >= E.numRows){
            if(E.numRows == 0 && y == E.screenrows / 3){
                char welcome[80];
                int welcomeLen = snprintf(welcome, sizeof(welcome), "SP Text Editor -- Version %s", SPTE_VERSION);
                if(welcomeLen > E.screencols) welcomeLen = E.screencols;
                int padding = (E.screencols - welcomeLen) / 2;
                if (padding){
                    abAppend(ab, "~", 1);
                    padding--;
                }
                while (padding--) abAppend(ab, " ", 1);
                abAppend(ab, welcome, welcomeLen);
            }else{
                abAppend(ab, "~", 1);
            }
        }else{
            int len = E.row[filerow].rsize - E.colOff;
            if(len < 0) len = 0;
            if(len > E.screencols) len = E.screencols;
            char *c = &E.row[filerow].render[E.colOff];
            unsigned char *hl = &E.row[filerow].hl[E.colOff];
            int currentColour = -1;
            int j;
            for(j = 0; j < len; j++){
                if(iscntrl(c[j])){
                    char sym = (c[j] <= 26) ? '@' + c[j] : '?';
                    abAppend(ab, "\x1b[7m", 4);
                    abAppend(ab, &sym, 1);
                    abAppend(ab, "\x1b[m", 3);
                    if(currentColour != -1){
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", currentColour);
                        abAppend(ab, buf, clen);
                    }
                }else if(hl[j] == HL_NORMAL){
                    if(currentColour != -1){
                        abAppend(ab, "\x1b[39m", 5);
                        currentColour = -1;
                    }
                    abAppend(ab, &c[j], 1);
                }else{
                    int colour = syntaxToColour(hl[j]);
                    if(colour != currentColour){
                        currentColour = colour;
                        char buf[16];
                        int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", colour);
                        abAppend(ab, buf, clen);
                    }
                    abAppend(ab, &c[j], 1);
                }
            }
            abAppend(ab, "\x1b[39m", 5);
        }

        abAppend(ab, "\x1b[K", 3);
        abAppend(ab, "\r\n", 2);
    }
}
 
void drawStatusBar(struct abuf *ab){
    abAppend(ab,"\x1b[7m", 4);
    char status[80], rstatus[90];
    int len = snprintf(status, sizeof(status), "%.20s - %d lines %s", E.filename ? E.filename : "[No Name]", E.numRows, E.dirty ? "(modified)" : "");
    int rlen = snprintf(rstatus, sizeof(rstatus), "%s | %d/%d", E.syntax ? E.syntax->filetype : "no ft", E.cy + 1, E.numRows);
    if(len > E.screencols) len = E.screencols;
    abAppend(ab, status, len);
    while (len < E.screencols){
        if(E.screencols- len == rlen){
            abAppend(ab, rstatus, rlen);
            break;
        }else{
            abAppend(ab, " ", 1);
            len++;
        }
    }
    
    abAppend(ab,"\x1b[m", 3);
    abAppend(ab, "\r\n", 2);
}

void drawMessageBar(struct abuf *ab){
    abAppend(ab, "\x1b[K", 3);
    int msglen = strlen(E.statusmsg);
    if(msglen > E.screencols) msglen = E.screencols;
    if(msglen && time(NULL) - E.statusmsg_time < 5) abAppend(ab, E.statusmsg, msglen);
}

void updateLinenumIndent(){
    int digit;
    int numRows = E.numRows;

    if(numRows == 0){
        digit = 0;
        E.linenumIndent = 2;
        return;
    }

    digit = 1;
    while (numRows >= 10 ){
        numRows = numRows / 10;
        digit++;
    }
    E.linenumIndent = digit + 2;
    
}

void refreshScreen(){
    updateLinenumIndent();
    E.screencols = E.rawScreencols - E.linenumIndent;
    editorScroll();

    struct abuf ab = ABUF_INIT;

    abAppend(&ab, "\x1b[?25l", 6);
    abAppend(&ab, "\x1b[H", 3);

    drawRows(&ab);
    drawStatusBar(&ab);
    drawMessageBar(&ab);

    char buf[32];
    snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowOff) + 1, (E.rx - E.colOff) + 1 + E.linenumIndent);
    abAppend(&ab, buf, strlen(buf));

    abAppend(&ab, "\x1b[?25h", 6);

    write(STDOUT_FILENO, ab.b, ab.len);
    abFree(&ab);
}

void setStatusMessage(const char *fmt, ...){
    va_list ap;
    va_start(ap, fmt);
    vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
    va_end(ap);
    E.statusmsg_time = time(NULL);
}

/** input **/

char *editorPrompt(char *prompt, void(*callback)(char*, int)){
    size_t bufsize = 128;
    char *buf = malloc(bufsize);

    size_t buflen = 0;
    buf[0] = '\0';

    while (1) {
        setStatusMessage(prompt, buf);
        refreshScreen();

        int c = readKey();
        if (c == DEL_KEY || c == CTRL_KEY('h') || c == BACKSPACE) {
            if (buflen != 0) buf[--buflen] = '\0';
            } else if (c == '\x1b') {
            setStatusMessage("");
            if(callback) callback(buf, c);
            free(buf);
            return NULL;
        } else if (c == '\r') {
            if (buflen != 0) {
                setStatusMessage("");
                if(callback) callback(buf, c);
                return buf;
            }
        } else if (!iscntrl(c) && c < 128) {
            if (buflen == bufsize - 1) {
                bufsize *= 2;
                buf = realloc(buf, bufsize);
            }
        buf[buflen++] = c;
        buf[buflen] = '\0';
        }
        if(callback) callback(buf, c);
    }
}

void moveCursor(int key){
    erow *row = (E.cy >= E.numRows) ? NULL : &E.row[E.cy];
    switch (key){
        case ARROW_LEFT:
            if(E.cx != 0){
                E.cx--;
            }else if(E.cy > 0){
                E.cy--;
                E.cx = E.row[E.cy].size;
            }
            break;
        case ARROW_RIGHT:
            if(row && E.cx < row->size){
                E.cx++;
            }else if (row && E.cx == row->size){
                E.cy++;
                E.cx = 0;
            }
            break;
        case ARROW_UP:
            if(E.cy != 0){
                E.cy--;
            }
            break;
        case ARROW_DOWN:
            if(E.cy < E.numRows){
                E.cy++;
            }
            break;
    }

    row = (E.cy >= E.numRows) ? NULL : &E.row[E.cy];
    int rowlen = row ? row->size : 0;
    if(E.cx > rowlen){
        E.cx = rowlen;
    }
}

void processKey(){
    static int quitTimes = QUIT_CONFIRMATION;

    int c = readKey();

    switch (c){
        case '\r':
            insertNewLine();
            break;
        
        case CTRL_KEY('q'):
            if(E.dirty && quitTimes > 0){
                setStatusMessage("WARNING! Unsaved Changes -- Press Ctrl-Q %d more time(s) to quit.", quitTimes);
                quitTimes--;
                return;
            }
            write(STDOUT_FILENO, "\x1b[2J", 4);
            write(STDOUT_FILENO, "\x1b[H", 3);
            exit(0);
            break;
        
        case CTRL_KEY('s'):
            saveEditor();
            break;

        case HOME_KEY:
            E.cx = 0;
            break;
        
        case END_KEY:
            if(E.cy < E.numRows){
                E.cx = E.row[E.cy].size;
            }
            break;
        
        case CTRL_KEY('f'):
            searchEditor();
            break;
        
        case BACKSPACE:
        case CTRL_KEY('h'):
        case DEL_KEY:
            if(c == DEL_KEY) moveCursor(ARROW_RIGHT);
            delChar();
            break;
        
        case PAGE_UP:
        case PAGE_DOWN:
            {
                if(c == PAGE_UP){
                    E.cy = E.rowOff;
                }else if(c == PAGE_DOWN){
                    E.cy = E.rowOff + E.screenrows - 1;
                    if(E.cy > E.numRows) E.cy = E.numRows;
                }   

                int times = E.screenrows;
                while (times--) moveCursor(c == PAGE_UP ? ARROW_UP : ARROW_DOWN);
            }
            break;

        case ARROW_UP:
        case ARROW_DOWN:
        case ARROW_LEFT:
        case ARROW_RIGHT:
            moveCursor(c);
            break;
        
        case CTRL_KEY('l'):
        case '\x1b':
            break;
        
        default:
            insertChar(c);
            break;
    }
    quitTimes = QUIT_CONFIRMATION;
}

/** init **/

void initEditor(){
    E.cx = 0;
    E.cy = 0;
    E.rx = 0;
    E.rowOff = 0;
    E.colOff = 0;
    E.numRows = 0;
    E.row = NULL;
    E.filename = NULL;
    E.dirty = 0;
    E.statusmsg[0] = '\0';
    E.statusmsg_time = 0;
    E.syntax = NULL;
    E.linenumIndent = 6;


    if(getWinSize(&E.rawScreenrows, &E.rawScreencols) == -1) kill("getWinSize");
    E.screenrows = E.rawScreenrows - 2;
    E.screencols = E.rawScreencols;
}

int main(int argc, char *argv[]){
    enableRaw();
    initEditor();
    if(argc >= 2){
        openEditor(argv[1]);
    }
    
    setStatusMessage("Ctrl-S = Save | Ctrl-Q = Quit | Ctrl-F = Search");

    while(1){
        refreshScreen();
        processKey();
    }
    return 0;
}
