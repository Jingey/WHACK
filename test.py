from assembler import Assembler, ParamError, OpcodeError, LineTooLongError
from cpu_design import build_computer

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

    computer = build_computer(formatted_lines, True)
    print("BEGINING EXECUTION")
    computer.run()


def build(filepath):
    assembler = Assembler(filepath)
    try:
        filepath = assembler.run()
    except (ParamError, OpcodeError, LineTooLongError) as e:
        raise e
    except Exception as e:
        raise e

    build_bin_file(filepath)


if __name__ == "__main__":
    build("test.txt")
