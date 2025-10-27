pub mod cpu;
pub mod mem;

pub mod tests {
    pub use crate::cpu::CPU6502;
    
    #[test]
    pub fn it_works() {
        let _ = CPU6502::new();
    }
}