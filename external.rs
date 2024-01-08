const TIOCGWINSZ: u32 = 0x5413;
const STDIN: i32 = 0;

#[repr(C)]
struct winsize {
    ws_row: u16,
    ws_col: u16,
    ws_xpixel: u16,
    ws_ypixel: u16,
}

extern "C" {
    fn ioctl(fd: i32, request: u32, ts: *mut winsize) -> i32;
}

pub struct TermSize {
    pub width: u16,
    pub height: u16,
}

pub fn get_terminal_size() -> Option<TermSize> {
    let mut tsize = winsize {
        ws_row: 0, ws_col: 0,
        ws_xpixel: 0, ws_ypixel: 0,
    };
    unsafe {
        let res = ioctl(STDIN, TIOCGWINSZ, &mut tsize as *mut winsize);
        if res != 0 {
            return None;
        }
        return Some(TermSize {
            width: tsize.ws_col,
            height: tsize.ws_row,
        });
    }
}
