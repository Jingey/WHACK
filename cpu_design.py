from alu import Alu
from wires import Register, Bus, Wire, BusCopier, Incrementer, FeStatus
from main_storage import MainStorage
from inout import Input, Output
from cu import Cu
from computer import Computer


def build_computer(data: list[int]) -> Computer:
    main_bus = Bus("main_bus")
    clock = Wire("clock")
    halt = Wire("halt")

    cu_bus = Bus("cu_bus")
    cu_bus_output = Wire("cu_bus_output")
    BusCopier(cu_bus, main_bus, cu_bus_output)

    func_bus = Bus("alu_func_bus")
    alu_enable = Wire("alu_enable")

    acc_read = Wire("acc_read")
    acc_write = Wire("acc_write")
    acc = Register("acc", main_bus, main_bus, acc_read, acc_write)

    alu = Alu(acc, main_bus, func_bus, alu_enable)

    r1_read = Wire("r1_read")
    r1_write = Wire("r1_write")
    r1 = Register("r1", main_bus, main_bus, r1_read, r1_write)

    r2_read = Wire("r2_read")
    r2_write = Wire("r2_write")
    r2 = Register("r2", main_bus, main_bus, r2_read, r2_write)

    main_store_enable = Wire("main_store_enable")
    addr_bus = Bus("addr_bus")
    mar_enable = Wire("mar_enable")
    BusCopier(main_bus, addr_bus, mar_enable)
    rw_bus = Bus("rw_bus")
    main_store = MainStorage(main_store_enable, addr_bus, main_bus, rw_bus)

    cir_read = Wire("cir_read")
    cir_write = Wire("cir_write")
    cir = Register("cir", main_bus, main_bus, cir_read, cir_write)

    pc_read = Wire("pc_read")
    pc_write = Wire("pc_write")
    pc = Register("pc", main_bus, main_bus, pc_read, pc_write)
    pc_increment = Wire("pc_increment")
    Incrementer(pc, pc_increment)

    read_input = Wire("read_input")
    input_reg = Input(read_input, main_bus)

    write_output = Wire("write_input")
    output_reg = Output(write_output, main_bus)

    ccr = alu.ccr

    fe_status = FeStatus(clock)

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
    # ccr - Bus
    # fe_status - access with fe_status.data - prefetch(0), fetch(1), execute_1(2), execute_2(3)

    cu = Cu(
        cu_bus=cu_bus,
        cu_bus_output=cu_bus_output,
        func_bus=func_bus,
        alu_enable=alu_enable,
        acc_read=acc_read,
        acc_write=acc_write,
        r1_read=r1_read,
        r1_write=r1_write,
        r2_read=r2_read,
        r2_write=r2_write,
        mar_enable=mar_enable,
        main_store_enable=main_store_enable,
        rw_bus=rw_bus,
        cir_read=cir_read,
        cir_write=cir_write,
        pc_read=pc_read,
        pc_write=pc_write,
        pc_increment=pc_increment,
        read_input=read_input,
        write_output=write_output,
        halt=halt,
        cir=cir,
        clock=clock,
        ccr=ccr,
        fe_status=fe_status,
    )

    main_store.load(data)
    return Computer(clock, halt, input_reg, output_reg)
