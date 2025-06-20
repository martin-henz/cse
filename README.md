# Reconstructing CSE
This repo contains the SML programs included in the paper
"Reconstructing a Notional Machine for Modern Scripting Languages".

## Features
The programs in the `src` folder all run in Moscow ML. They are
extended versions of the programs in the paper, typically by
adding support for integers so that more interesting example
programs can run.

## Requirements
You will need a working SML implementation. I've tested the programs
using Moscow ML version 2.10.

## Usage
Clone the repo and run the programs separately in Moscow ML:
```bash
cd src
mosml cse.ml
```

The programs contain some test cases at the end. You can run all programs
with:
```bash
cd src
make
```

If the make command fails with permission errors, first run:
```bash
chmod +x test.sh
```

If you use other SML implementations, you'll need to edit `src/Makefile`
appropriately.

## License
This project is licensed under the Apache 2.0 License - see the [LICENSE](LICENSE) file for details.

## Todo
- More test cases
- Programs for sections 3.2, 3.3, and 3.4 need to be added and tested.

## Contact
Martin Henz - henz@comp.nus.edu.sg
