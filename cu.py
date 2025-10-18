from enum import Enum
from alu import ALUFunction
from wires import Wire, Bus, Register


class Opcode(Enum):
    NOP = 0b0000
    JMP = 0b0001
    JEZ = 0b0010
    JNV = 0b0011
    STR = 0b0100
    LDR = 0b0101
    MOV = 0b0110
    LS = 0b0111
    ADD = 0b1000
    SUB = 0b1001
    AND = 0b1010
    OR = 0b1011
    NOT = 0b1100
    INP = 0b1101
    OUT = 0b1110
    HLT = 0b1111


class Cu:
    def __init__(
        self,
        cu_bus: Bus,
        cu_bus_output: Wire,
        func_bus: Bus,
        alu_enable: Wire,
        acc_read: Wire,
        acc_write: Wire,
        r1_read: Wire,
        r1_write: Wire,
        r2_read: Wire,
        r2_write: Wire,
        mar_enable: Wire,
        main_store_enable: Wire,
        rw_bus: Bus,
        cir_read: Wire,
        cir_write: Wire,
        pc_read: Wire,
        pc_write: Wire,
        pc_increment: Wire,
        read_input: Wire,
        write_output: Wire,
        halt: Wire,
        cir: Register,
        clock: Wire,
        ccr: Bus,
        fe_status: Register,
    ):
        self.cu_bus = cu_bus
        self.cu_bus_output = cu_bus_output
        self.func_bus = func_bus
        self.alu_enable = alu_enable
        self.acc_read = acc_read
        self.acc_write = acc_write

        # General purpose registers
        self.r1_read = r1_read
        self.r1_write = r1_write
        self.r2_read = r2_read
        self.r2_write = r2_write

        self.mar_enable = mar_enable
        self.main_store_enable = main_store_enable
        self.rw_bus = rw_bus

        self.cir_read = cir_read
        self.cir_write = cir_write

        self.pc_read = pc_read
        self.pc_write = pc_write
        self.pc_increment = pc_increment

        self.read_input = read_input
        self.write_output = write_output

        self.halt = halt

        # Inputs
        self.cir = cir
        self.clock = clock
        self.ccr = ccr
        self.fe_status = fe_status

        self.clock.enlist(self.run)

        self.emulator = CuEmulator()

    def create_bit_str(self):
        return (self.cir.data << 5) | (self.ccr.data << 2) | self.fe_status.data

    def run(self):
        result = self.emulator.run(self.create_bit_str())
        self.handle_result(result)

    def handle_result(self, result):
        self.cu_bus.set_data(result >> 22)
        self.func_bus.set_data((result >> 19) & 0b111)
        cu_bus_output = (result >> 18) & 1
        alu_enable = (result >> 17) & 1
        acc_read = (result >> 16) & 1
        acc_write = (result >> 15) & 1

        # General purpose registers
        r1_read = (result >> 14) & 1
        r1_write = (result >> 13) & 1
        r2_read = (result >> 12) & 1
        r2_write = (result >> 11) & 1

        mar_enable = (result >> 10) & 1
        main_store_enable = (result >> 9) & 1
        rw_bus = (result >> 8) & 1

        cir_read = (result >> 7) & 1
        cir_write = (result >> 6) & 1

        pc_read = (result >> 5) & 1
        pc_write = (result >> 4) & 1
        pc_increment = (result >> 3) & 1

        read_input = (result >> 2) & 1
        write_output = (result >> 1) & 1

        halt = (result >> 0) & 1

        if halt:
            self.halt.enable()
            return

        if rw_bus == 1:
            self.rw_bus.set_data(rw_bus)

        # All writes to main bus
        if cu_bus_output == 1:
            self.cu_bus_output.enable()
        if read_input == 1:
            self.read_input.enable()
        if rw_bus == 1 and main_store_enable == 1:
            self.main_store_enable.enable()
        if acc_write == 1:
            self.acc_write.enable()
        if r1_write == 1:
            self.r1_write.enable()
        if r2_write == 1:
            self.r2_write.enable()
        if pc_write == 1:
            self.pc_write.enable()
        if cir_write == 1:
            self.cir_write.enable()

        if alu_enable == 1:
            self.alu_enable.enable()

        # All reads off of main bus last
        if acc_read == 1:
            self.acc_read.enable()
        if r1_read == 1:
            self.r1_read.enable()
        if r2_read == 1:
            self.r2_read.enable()
        if pc_read == 1:
            self.pc_read.enable()
        if cir_read == 1:
            self.cir_read.enable()
        if write_output == 1:
            self.write_output.enable()
        if mar_enable == 1:
            self.mar_enable.enable()
        if rw_bus == 0 and main_store_enable == 1:
            self.main_store_enable.enable()

        if pc_increment == 1:
            self.pc_increment.enable()


class CuEmulator:
    def __init__(self):
        pass

    # CIR 16 bits | CCR 3 bits | FE_Status 2 bits
    # returns 38 bits
    def run(self, bit_str: int):
        # Inputs
        self.cir = bit_str >> 5
        self.ccr = (bit_str >> 2) & 0b111
        self.fe_status = bit_str & 0b11

        # Outputs

        self.cu_bus = 0
        self.func_bus = 0
        self.cu_bus_output = 0
        self.alu_enable = 0
        self.acc_read = 0
        self.acc_write = 0

        # General purpose registers
        self.r1_read = 0
        self.r1_write = 0
        self.r2_read = 0
        self.r2_write = 0

        self.mar_enable = 0
        self.main_store_enable = 0
        self.rw_bus = 0

        self.cir_read = 0
        self.cir_write = 0

        self.pc_read = 0
        self.pc_write = 0
        self.pc_increment = 0

        self.read_input = 0
        self.write_output = 0

        self.halt = 0

        self.run_cycle()

        base = self.bit_pack(
            [
                self.cu_bus_output,
                self.alu_enable,
                self.acc_read,
                self.acc_write,
                self.r1_read,
                self.r1_write,
                self.r2_read,
                self.r2_write,
                self.mar_enable,
                self.main_store_enable,
                self.rw_bus,
                self.cir_read,
                self.cir_write,
                self.pc_read,
                self.pc_write,
                self.pc_increment,
                self.read_input,
                self.write_output,
                self.halt,
            ]
        )

        return (self.cu_bus << 22) | (self.func_bus << 19) | base

    def bit_pack(self, res: list[int]):
        result = 0
        for i in res:
            result <<= 1
            result |= i
        return result

    def run_cycle(self):
        match self.fe_status:
            case 0:
                self.prefetch()
            case 1:
                self.fetch()
            case 2:
                self.execute_1()
            case 3:
                self.execute_2()

    def prefetch(self):
        # Send pc to mar.
        self.pc_write = 1
        self.mar_enable = 1

        # Increment pc
        self.pc_increment = 1

    def fetch(self):
        self.rw_bus = 1
        self.main_store_enable = 1

        self.cir_read = 1

    def execute_1(self):
        instruction = self.cir
        op_code = instruction >> 12
        operand = instruction & 0b1111_11111111

        match op_code:
            case Opcode.HLT.value:
                self.halt_1()
            case Opcode.JMP.value:
                self.jmp_1(operand)
            case Opcode.JEZ.value:
                self.jmp_ez_1(operand)
            case Opcode.JNV.value:
                self.jmp_nvg_1(operand)
            case Opcode.STR.value:
                self.str_1(operand)
            case Opcode.LDR.value:
                self.ldr_1(operand)
            case Opcode.MOV.value:
                self.mov_1(operand)
            case Opcode.LS.value:
                self.ls_1(operand)
            case Opcode.ADD.value:
                self.add_1(operand)
            case Opcode.SUB.value:
                self.sub_1(operand)
            case Opcode.AND.value:
                self.and_1(operand)
            case Opcode.OR.value:
                self.or_1(operand)
            case Opcode.NOT.value:
                self.not_1(operand)
            case Opcode.INP.value:
                self.inp_1(operand)
            case Opcode.OUT.value:
                self.out_1(operand)

            case _:
                # NOP
                return

    def execute_2(self):
        instruction = self.cir
        op_code = instruction >> 12
        operand = instruction & 0b1111_11111111
        match op_code:
            case Opcode.STR.value:
                self.str_2(operand)
            case Opcode.LDR.value:
                self.ldr_2(operand)

    def halt_1(self):
        self.halt = 1

    def write_to_main_bus(self, data):
        self.cu_bus = data
        self.cu_bus_output = 1

    def jmp_1(self, operand):
        # load the address onto the main bus
        self.write_to_main_bus(operand)
        # read the main bus into the program counter
        self.pc_read = 1

    def jmp_ez_1(self, operand):
        # if last calculation is zero
        if self.ccr & 0b100 != 0:
            self.jmp_1(operand)

    def jmp_nvg_1(self, operand):
        # if last calculation is negative
        if self.ccr & 0b001 != 0:
            self.jmp_1(operand)

    def send_to_register(self, destination_register):
        if destination_register == 0:
            self.r1_read = 1
        else:
            self.r2_read = 1

    def send_from_register(self, source_register):
        if source_register == 0:
            self.r1_write = 1
        else:
            self.r2_write = 1

    def load_into_mar(self, addr):
        self.write_to_main_bus(addr)
        self.mar_enable = 1

    def load_acc_into_mar(self):
        self.acc_write = 1
        self.mar_enable = 1

    def str_ldr_address_handling(self, operand):
        addr = operand & ((1 << 11) - 1)

        if addr == 0b111_11111111:
            self.load_acc_into_mar()
        else:
            self.load_into_mar(addr)

    def str_1(self, operand):
        self.str_ldr_address_handling(operand)

    def str_2(self, operand):
        self.rw_bus = 0
        self.send_from_register((operand >> 11) & 1)

        self.main_store_enable = 1

    def ldr_1(self, operand):
        self.str_ldr_address_handling(operand)

    def ldr_2(self, operand):
        self.rw_bus = 1
        self.main_store_enable = 1

        self.send_to_register((operand >> 11) & 1)

    # from first -> second
    def mov_1(self, operand):
        first_reg = (operand >> 10) & 0b11
        second_reg = (operand >> 8) & 0b11

        if second_reg == 0:
            return

        match first_reg:
            case 0b00:
                self.write_to_main_bus(0)
            case 0b01:
                self.acc_write = 1
            case 0b10:
                self.r1_write = 1
            case 0b11:
                self.r2_write = 1

        match second_reg:
            case 0b01:
                self.acc_read = 1
            case 0b10:
                self.r1_read = 1
            case 0b11:
                self.r2_read = 1

    def ls_1(self, operand):
        shift_amount = operand >> (12 - 5)
        self.write_to_main_bus(shift_amount)
        self.func_bus = ALUFunction.SHIFT.value
        self.alu_enable = 1

    def binary_operation(self, function, operand):
        form = operand >> 11
        if form == 0:
            self.send_from_register((operand >> 10 & 0b1))
        else:
            self.write_to_main_bus(operand & ((1 << 11) - 1))

        self.func_bus = function.value
        self.alu_enable = 1

    def add_1(self, operand):
        self.binary_operation(ALUFunction.ADD, operand)

    def sub_1(self, operand):
        self.binary_operation(ALUFunction.SUB, operand)

    def and_1(self, operand):
        self.binary_operation(ALUFunction.AND, operand)

    def or_1(self, operand):
        self.binary_operation(ALUFunction.OR, operand)

    def not_1(self, operand):
        self.func_bus = ALUFunction.NOT.value
        self.alu_enable = 1

    def inp_1(self, operand):
        self.read_input = 1

        self.send_to_register((operand >> 11) & 1)

    def out_1(self, operand):
        self.send_from_register((operand >> 11) & 1)

        self.write_output = 1
