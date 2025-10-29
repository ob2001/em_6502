pub mod prelude;
pub mod cpu;
pub mod mem;

pub type CPUByte = u8;
pub type CPUWord = u16;

pub const CPU_MEMSIZE: usize = 65536;