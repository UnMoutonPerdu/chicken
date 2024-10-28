# Chicken

A compiler from Chicken to a certain ByteCode of a certain VM.

## Commands

Basic command to setup your repo:
```
make clean && make depend && make
```

Then run the compiler using:
```
./ckc "name_of_your_chicken_file"
```

And then the interpreter:
```
./ckrun a.out
```

