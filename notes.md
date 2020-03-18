# Notes for project 2
### Layers
- Instead of a state that exists with vars and values `(ie ((a b c foo)(1 2 34))`, we will make a set of blocked layers

```
if (a == b)  --------
	body         |
end                  |
                     |
x = 2 ---------------+------------
    		     v	         v      
		(((a b)(1 2)) ((x)(2)))
```
