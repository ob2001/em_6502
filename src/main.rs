use em_6502::{cpu::CPU6502, mem};

fn main() {
    let mut mem = mem::mem_with_code_at(&[0xA9, 0xF3, 0x00, 0xFF], 0).unwrap();
    mem::mem_insert_code_at(&mut mem, &[0x03, 0x00], 0xFFFE).unwrap();

    let mut cpu = CPU6502::new_with_mem(mem);
    cpu.power_on_and_run(true);

    // Print cpu final state and dump memory
    if false {
        println!("{:?}", cpu);
    }
}
