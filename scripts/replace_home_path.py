import re

if __name__=="__main__":
    with open("binaries.txt", "r") as f:
        lines=f.readlines()
        newlines=[re.sub(r"/home/[a-zA-Z]*", r"${HOME}", s) for s in lines]
        with open("binaries.txt", "w") as fnew:
            fnew.writelines(newlines)
