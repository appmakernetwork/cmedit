/*
 * cmedit_term.c - tiny C helpers that are awkward to express through the
 * Haskell `unix` package alone. Currently just the TIOCGWINSZ ioctl used to
 * query the terminal window size in character cells.
 */
#include <sys/ioctl.h>
#include <unistd.h>

/*
 * Fill *rows and *cols with the current size of the terminal attached to fd.
 * Returns 0 on success, -1 on failure (in which case *rows/*cols are
 * untouched and the caller should fall back to a default or $LINES/$COLUMNS).
 */
int cmedit_get_winsize(int fd, int *rows, int *cols)
{
    struct winsize ws;
    if (ioctl(fd, TIOCGWINSZ, &ws) == -1) {
        return -1;
    }
    if (ws.ws_row == 0 || ws.ws_col == 0) {
        return -1;
    }
    *rows = ws.ws_row;
    *cols = ws.ws_col;
    return 0;
}

/*
 * Fill *xpx and *ypx with the terminal's total text-area size in pixels, if
 * the kernel knows it. Many terminals leave ws_xpixel/ws_ypixel zero; this
 * returns -1 then and the caller falls back to the XTWINOPS 14/16 queries.
 */
int cmedit_get_winsize_px(int fd, int *xpx, int *ypx)
{
    struct winsize ws;
    if (ioctl(fd, TIOCGWINSZ, &ws) == -1) {
        return -1;
    }
    if (ws.ws_xpixel == 0 || ws.ws_ypixel == 0) {
        return -1;
    }
    *xpx = ws.ws_xpixel;
    *ypx = ws.ws_ypixel;
    return 0;
}
