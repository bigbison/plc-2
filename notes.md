# Notes for project 2
### Layers
- Instead of a state that exists with vars and values `(ie ((a b c foo)(1 2 3 4))`, we will make a set of layers divided by block (if, main, etc.)

```
if (a == b)  --------
	body         |
end                  |
                     |
x = 2 ---------------+------------
    		     v	         v      
		(((a b)(1 2)) ((x)(2)))
```
