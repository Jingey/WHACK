from cpu_design import build_computer
from assembler import Assembler, ParamError, OpcodeError, LineTooLongError


"""
Given assembly file, or assemble binary. Do the assemble yo, then build computer, load it into memory, 
return computer.
"""


def build_bin_file(filepath):
    formatted_lines = []
    with open(filepath, "rb") as f:
        lines = f.readlines()
        for line in lines:
            formatted_lines.append(int(line, 2))

    print("STUFF")
    computer = build_computer(formatted_lines, True)
    print("BEGINING EXECUTION")
    computer.run()


def build(filepath):
    print("AAA")
    assembler = Assembler(filepath)
    try:
        print("NNJM")
        filepath = assembler.run()
    except (ParamError, OpcodeError, LineTooLongError) as e:
        print("hst")
        print(e)
        raise e
    except Exception as e:
        print("Bjn")
        print(e)
        raise e
    print("jnvbjv")
    build_bin_file(filepath)


if __name__ == "__main__":
    print("bjn nm")
    build("test.txt")
