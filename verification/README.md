
# Verification

## Setup
The following commands should have been run (rtl compiled and boot file generation) for the verification commands to be run.
```
$ cd c-class 
$ make
$ make generate_boot_files
```
All the commands have to be run from SHAKTI_C_HOME: <repo-path>/c-class

## Examples Cheat Sheet


| command          | description |
| -----------------| ---------------- |
| make regress                                          |  lists the tests|
| make regress opts="--clean"                           | cleans| 
| make regress opts="--filter=rv64mi"                   | regx filter| 
| make regress opts="--filter=rv64mi --sub"             | runs filtered tests| 
| make regress opts="--filter=mulh --parallel --sub"    | runs in parallel| 
| make regress opts="--gen --sub --test_count=5" | generates & runs random tests along with riscv-tests| 
| make torture opts="--test_count=20 --parallel" | generates 20 riscv-torture tests with bringup.config in parallel|
|make torture opts="--sub"| generates and simulates a single riscv-torture test|

## Detailed Command Description
1. Lists the test
```
$ make regress

[makeRegress.pl] Regression run ------------
         directed/riscv-tests/isa/rv64mi                                      csr     p    NOT_RUN
         directed/riscv-tests/isa/rv64mi                                     mcsr     p    NOT_RUN
         directed/riscv-tests/isa/rv64mi                                  ma_addr     p    NOT_RUN
         directed/riscv-tests/isa/rv64mi                                    scall     p    NOT_RUN
         directed/riscv-tests/isa/rv64si                                      csr     p    NOT_RUN
         directed/riscv-tests/isa/rv64si                                    scall     p    NOT_RUN
         directed/riscv-tests/isa/rv64ua                                 amoadd_d     p    NOT_RUN
         directed/riscv-tests/isa/rv64ua                                 
```

2. Regression clean
```
$ make regress opts="--clean"

[makeRegress.pl] Regression run ------------
[makeRegress.pl] Cleaning...
[makeRegress.pl] 'rm -rf /scratch/lavanya/c-class/verification/workdir/*'
[makeRegress.pl] 'rm -rf /scratch/lavanya/c-class/verification/scripts/nohup.out'
[makeRegress.pl] 'rm -rf /scratch/lavanya/c-class/verification/tools/AAPG/nohup.out'
[makeRegress.pl] 'rm -rf /scratch/lavanya/c-class/verification/tools/AAPG/__pycache__'
[makeRegress.pl] 'rm -rf /scratch/lavanya/c-class/verification/tests/random/*/generated_tests/*'

```
3. Filter the tests
```
$ make regress opts="--filter=rv64mi"

[makeRegress.pl] Regression run ------------
         directed/riscv-tests/isa/rv64mi                                      csr     p    NOT_RUN
         directed/riscv-tests/isa/rv64mi                                     mcsr     p    NOT_RUN
         directed/riscv-tests/isa/rv64mi                                  ma_addr     p    NOT_RUN
         directed/riscv-tests/isa/rv64mi                                    scall     p    NOT_RUN

```

4. Run only the filtered tests
```
$ make regress opts="--filter=rv64mi --sub"
```

5. Run all the riscv-tests (tests are run one after the other)
```
$ make regress opts="--sub"
```

6. Run the tests in parallel. Currently this option runs at max 50 licenses in parallel. So it is advisable to use this option with caution
```
$ make regress opts="--filter=mulh --parallel --sub"
```
You can check the results of the above submission with the below command
```
$ make regress opts="--filter=mulh"
```

7. Generate random tests during regression. 5 tests of each configuration is generated
```
$ make regress opts="--gen --sub --test_count=5"
```
8. Generate riscv-torture tests
```
$ make torture opts="--config=smoke --test_count=3 --parallel"
```
Wait till ``[success] Total time`` for all tests. The generated tests will be present in:
```
$SHAKTI_C_HOME/verification/tests/random/riscv-torture/generated_tests/<config_name> 
```
9. Generate and simulate a single riscv-torture test
```
$ make torture opts="--sub"
```
