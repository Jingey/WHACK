from alu import Alu
from wires import Register, Bus, Wire, BusSwitch, BusCopier
from main_storage import MainStorage
from inout import Input, Output
from computer import Computer


def build_computer(data: list[int]) -> Computer:
    main_bus = Bus()
    clock = Wire()
    halt = Wire()

    cu_bus = Bus()
    cu_bus_output = Wire()
    BusCopier(cu_bus, main_bus, cu_bus_output)

    acc_bus = Bus()
    func_bus = Bus()
    alu_enable = Wire()
    alu_out_bus = Bus()

    alu = Alu(acc_bus, main_bus, func_bus, alu_enable, alu_out_bus)

    acc_in_select = Bus()
    acc_in = BusSwitch([main_bus, alu_out_bus])

    acc_out_select = Bus()
    acc_out = BusSwitch([main_bus, acc_bus])

    acc_read = Wire()
    acc_write = Wire()
    acc = Register(acc_in, acc_out, acc_read, acc_write)

    r1_read = Wire()
    r1_write = Wire()
    r1 = Register(main_bus, main_bus, r1_read, r1_write)

    r2_read = Wire()
    r2_write = Wire()
    r2 = Register(main_bus, main_bus, r2_read, r2_write)

    main_store_enable = Wire()
    addr_bus = Bus()
    mar_enable = Wire()
    BusCopier(main_bus, addr_bus, mar_enable)
    rw_bus = Bus()
    main_store = MainStorage(main_store_enable, addr_bus, main_bus, rw_bus)

    cir_read = Wire()
    cir_write = Wire()
    cir = Register(main_bus, main_bus, cir_read, cir_write)

    pc_read = Wire()
    pc_write = Wire()
    pc = Register(main_bus, main_bus, pc_read, pc_write)

    read_input = Wire()
    input_reg = Input(read_input, main_bus)

    write_output = Wire()
    output_reg = Input(write_output, main_bus)

    # CU out connections:
    # cu_bus - Bus: to send data to main bus (e.g. constants)
    # cu_bus_output - Wire: enables cu_bus and copies what is on it to main_bus
    #
    # func_bus - Bus: connects function lines to the ALU
    # alu_enable - Wire: runs the ALU putting the output on the alu_out bus
    #
    # acc_in_select - Bus: selects where the acc reads from (either main_bus (0) or alu_out (1))
    # acc_out_select - Bus: selects where the acc writes to (either the main_bus (0) or the alu_input(1))
    # acc_read - Wire: copies what is on the acc read bus into the acc
    # acc_write - Wire: copies the acc into the acc write bus
    #
    # r1 and r2 read/write - Wires: reads/writes from main bus into each register
    #
    # main_store_enable - Wire: perfroms a read/write operation in the main store putting the result onto the main bus
    # rw_bus - Bus: selects main store function (0 - write, 1 - read)
    # mar_enable - Wire: copies the main bus into the mar, this is the address used for memory operations
    #
    # cir_read - Wire: reads form main bus into CIR
    # cir_write - Wire: writes form CIR into main bus
    #
    # pc_read - Wire: reads form main bus into PC
    # pc_write - Wire: writes form PC into main bus
    # pc_increment - Wire: increments the PC
    #
    # read_input - Wire: copies one peice of the input queue onto the main bus
    # write_output - Wire: copies the main bus to the output
    #
    # halt - Wire: stops the CPU and no further running

    # CU in connections:
    # cir - Register
    # clock - Wire: enabled to trigger each execution
    # ccr - Register

    main_store.load(data)
    return Computer(clock, halt, input_reg, output_reg)
