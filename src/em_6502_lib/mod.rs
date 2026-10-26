pub mod prelude;
pub mod cpu;
pub mod mem;
pub mod bits;

pub type Byte = u8;
pub type Word = u16;

pub const CPU_MEMSIZE: usize = 65536;