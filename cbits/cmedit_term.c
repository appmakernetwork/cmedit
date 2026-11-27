/*
 * cmedit_term.c - tiny C helpers that are awkward to express through the
 * Haskell `unix` package alone: TIOCGWINSZ ioctls for terminal size and
 * POSIX shared-memory helpers for kitty graphics t=s transport.
 */
#include <errno.h>
#include <fcntl.h>
#include <stddef.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
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

/*
 * Create a fresh owner-only (0600) POSIX shared-memory object with O_EXCL,
 * fill it with len bytes from data, and close the fd.
 *
 * Returns:
 *   0  on success
 *   1  if an object with that name already existed (EEXIST: caller may retry)
 *  -1  on any other error (any created object is unlinked before returning)
 */
int cmedit_shm_write(const char *name, const unsigned char *data, size_t len)
{
    int fd = shm_open(name, O_CREAT | O_EXCL | O_RDWR, 0600);
    if (fd < 0) {
        return (errno == EEXIST) ? 1 : -1;
    }
#ifdef __APPLE__
    /* macOS shm_open fds reject write(2); populate via ftruncate + mmap. */
    if (len > 0) {
        if (ftruncate(fd, (off_t)len) < 0) {
            close(fd);
            shm_unlink(name);
            return -1;
        }
        void *mem = mmap(NULL, len, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
        if (mem == MAP_FAILED) {
            close(fd);
            shm_unlink(name);
            return -1;
        }
        memcpy(mem, data, len);
        munmap(mem, len);
    }
#else
    /* write(2) returns ENOSPC cleanly if /dev/shm is full (no SIGBUS). */
    size_t off = 0;
    while (off < len) {
        ssize_t n = write(fd, data + off, len - off);
        if (n < 0) {
            if (errno == EINTR) {
                continue;
            }
            close(fd);
            shm_unlink(name);
            return -1;
        }
        if (n == 0) {
            close(fd);
            shm_unlink(name);
            return -1;
        }
        off += (size_t)n;
    }
#endif
    if (close(fd) < 0) {
        shm_unlink(name);
        return -1;
    }
    return 0;
}

/*
 * Unlink a POSIX shared-memory object by name (best-effort; ENOENT is normal
 * once the terminal has already consumed it).
 */
int cmedit_shm_unlink(const char *name)
{
    return shm_unlink(name);
}
