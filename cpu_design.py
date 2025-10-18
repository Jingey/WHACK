from alu import Alu
from wires import Register, Bus, Wire, BusSwitch

main_bus = Bus()

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


# CU connections:
# main_bus
# func_bus
# alu_enable
# acc_in_select
# acc_out_select
# acc_read
# acc_write
