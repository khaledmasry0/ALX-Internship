# Built in functions

<h1 align="center"> String </h1>

### type() : Type of variables

```python
x = "python"
print(type(x))   # str ==> string
```

### len() : the length of the string

```python
x = "python"
print(len(x))   # 6
```

### format() || str() : convert to string

```python
x = 20
print("python " + format(x))   # python 20
x = 20
print("python " + str(x))      # python 20
```

### upper() : To Upper Case

```python
x = "python"
print(x.upper())   # PYTHON
```

### lower() : To Lower Case

```python
x = "PYTHON"
print(x.lower())   # python
```

### replace("first" , "second") : replace "first" by "second"

```python
x = "python"
print(x.replace("n" , "c"))   # pythoc
```

### count("x") : count "x" in string

```python
x = "python"
print(x.count("t"))   # 1
```

<hr/>

<h1 align="center"> Boolean </h1>

```python
x = True
print(type(x))   # bool ==> boolean

x = 10 > 2
print(x)   # True
```

<hr/>

<h1 align="center"> Number </h1>

```python
x = 10
print(type(x))   # int

x = 10.5
print(type(x))   # float
```

## casting

```python
x = 10
print(float(x))   # 10.0

x = 10.5
print(int(x))   # 10

x = "10"
print(int(x))   # 10 ==> int
```
