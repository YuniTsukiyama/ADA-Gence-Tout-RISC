#!/usr/bin/env python3

from argparse import ArgumentParser
from argparse import RawDescriptionHelpFormatter
from pathlib import Path
from difflib import unified_diff as diff
from termcolor import colored
import yaml
import os
import subprocess
import fnmatch
import difflib


DESCRIPTION = """Testsuite pour le projet d'ADA.
Ce projet consiste a l'implementation d'un emulateur d'un processeur asynchrone\n"""

EPILOG = """Make check pour lancer la testsuite.
\n
\n
Les tests se basent sur le fichier tests.yml. Si vous souhaitez en rajouter, voici les
options des .yml:\n
    - name : nom du test
    - output: stdout attendu (a la lettre pres)
    - return_code: return code attendu. Rien pour 0\n
    - options: les options donnes a AH_dacore.py. Le fichier .s a d'ada-corder est considere
      comme une option.\n
      \n
Pour plus d'informations, envoyer un mail a <cloe.lacombe@epita.fr> ou quelqu'un d'autre du groupe
 au mieux. J'ai les meme horaires que le service des impots."""


def print_synthesis(passed, failed):
    if (not passed + failed):
        ratio = 0
    else:
        ratio = failed / (passed + failed)
    print(f"[{colored('==', 'red' if ratio > 0.5 else 'magenta' if ratio <= 0.5 and ratio > 0.25 else 'yellow' if ratio <= 0.25 and ratio > 0 else 'blue')}] Synthesis:", end='')
    color_tested = colored(f"{passed + failed:2d}", 'blue')
    print(f" Tested: {color_tested} | ", end='')
    color_tested = colored(f"{passed:2d}", 'green')
    print(f"Passing: {color_tested} | ", end='')
    color_tested = colored(f"{failed:2d}", 'red' if failed else 'blue')
    print(f"Failing: {color_tested}")


def run_test(args, timeout=3):
    args = ["timeout", "--signal=KILL", f"{timeout}"] + args
    res = subprocess.run(args, capture_output=True, text=False)
    return res


def test(binary, test_case):
    binary = [binary]
    binary = binary + test_case.get("options", [])
    res_comp = run_test(binary)

    checks = test_case.get("checks", [])

    if ("stdout" in checks):
        actu_stdout = str(res_comp.stdout, "utf-8")
        expect_stdout = test_case.get("output", "")
        assert actu_stdout == expect_stdout, \
                f"Assembly badly assembled. Expected '{expect_stdout}', got '{actu_stdout}'"

    if ("has_stdout" in checks):
        actu_stdout = str(res_comp.stdout, "utf-8")
        assert actu_stdout != "", \
                f"Assembly badly assembled. Expected something on stdout, got nothing"

    if ("has_stderr" in checks):
        actu_stderr = str(res_comp.stderr, "utf-8")
        assert actu_stderr != "", \
                f"Assembly badly assembled. Expected something on stderr, got nothing"

    if ("returncode" in checks):
        expected_return = test_case.get("return_code", "")

        if (expected_return == ""):
            assert res_comp.returncode == 0, \
                    f"return code not valid.\nExpected 0, got {res_comp.returncode}"
        else:
            assert res_comp.returncode == int(expected_return), \
                    f"return code not valid.\nExpected {expected_return}, got {res_comp.returncode}"


def launch_one_test(binary, test_case):
    try:
        test(binary, test_case)
    except AssertionError as err:
        print(f"[{colored('KO', 'red')}]", test_case.get("name"))
        print(f"{err}")
        return 0

    print(f"[{colored('OK', 'green')}]", test_case.get("name"))
    return 1 


def launch_tests(binary):
    passed, failed = 0, 0
    test = ""

    for test_file in Path(os.path.dirname(__file__)).rglob('tests.yml'):
        test = test_file

    with open(test, "r") as fichier:
        print(f" {test} ".center(80, '-') + "\n")
        for test_case in yaml.safe_load(fichier):
            if (launch_one_test(binary, test_case)):
                passed += 1
            else:
                failed += 1

        print("\n" + " GLOBAL SYNTHESIS ".center(80, "-"))
        print_synthesis(passed, failed)
        print("".center(80, "-") + "\n")


if __name__ == "__main__":
    parser = ArgumentParser(description = DESCRIPTION, formatter_class = RawDescriptionHelpFormatter,
            epilog = EPILOG)
    parser.add_argument("bin", metavar="BINARY")
    args = parser.parse_args()
    binary = Path(args.bin).absolute()
    launch_tests(binary)
