from alu import Alu
from wires import Register, Bus, Wire, BusSwitch, BusCopier

main_bus = Bus()
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

# CU out connections:
# cu_bus
# cu_bus_output
# func_bus
# alu_enable
# acc_in_select
# acc_out_select
# acc_read
# acc_write
#
# r1 and r2 read/write
# memory r/w
# MAR_read
# memory_activate
#
# CIR read
# PC write
# PC read
# reset cycle register
# HALT wire

# CU in connections:
# CIR reg
# CCR reg
# clock line
# cylce register
