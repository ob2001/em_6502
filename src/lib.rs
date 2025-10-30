pub mod prelude;
pub mod cpu;
pub mod mem;
pub mod bitfield;
pub mod bitmasks;

pub type CPUByte = u8;
pub type CPUWord = u16;

pub const CPU_MEMSIZE: usize = 65536;