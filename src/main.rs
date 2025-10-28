use em_6502::cpu::{CPU6502, mem_with_code_at, mem_insert_code_at};

fn main() {
    let mut cpu = CPU6502::new(true);
    let mut mem = mem_with_code_at(&[0xA9, 0xFF, 0x00], 0).unwrap();
    let _ = mem_insert_code_at(&mut mem, &[0x00, 0x00, 0x01, 0x00], 0xFFFC);

    cpu.flash_mem(mem);
    cpu.power_on_and_run();

    // println!("{:?}", cpu);
}
