use em_6502::{cpu::CPU6502, mem};

fn main() {
    let mut mem = mem::new_nops_with_code_at(&[0xA9, 0xF3, 0x00, 0xFF], 0x0000).unwrap();
    mem::insert_code_at(&mut mem, &[0xFF], 0x0010).unwrap();
    mem::insert_code_at(&mut mem, &[0x00, 0x00, 0x10, 0x00], 0xFFFC).unwrap();

    let mut cpu = CPU6502::new_with_mem(mem);
    cpu.power_on_and_run(true);

    // Print CPU and memory postmortem
    if false {
        println!("vvv CPU and Memory Postmortem vvv");
        println!("{:?}", cpu);
    }
}
