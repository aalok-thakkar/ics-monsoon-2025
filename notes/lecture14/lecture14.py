
# All the functions we have built till now can be translated to python. 

# let is_prime (n: int) : bool =
#   if n < 2 then false
#   else
#     let i = ref 2 in
#     let has_divisor = ref false in
#     while !i * !i <= n && not !has_divisor do
#       if n mod !i = 0 then has_divisor := true;
#       i := !i + 1
#     done;
#     not !has_divisor
# ;;


def is_prime(n: int) -> bool:
    if n < 2:
        return False
    else:
        for i in range(2, n - 1): 
            if n % i == 0:
                return False
        return True









# Now let us see why Python is funny.

a = [1, 2, 3]
b = a
a += [4]
# print(b)

a = "1, 2, 3"
b = a
a += ", 4"
# print(b)






# Here, we see a difference. It is because the two objects are of different types.

from typing import List

a: List[int] = [1, 2, 3]
b: List[int] = a
a += [4]

# After a += [4], both a and b will be updated to [1, 2, 3, 4]
# print("a:", a)
# print("b:", b)

# Explanation: In this case, both 'a' and 'b' are referencing the same list in memory.
# So, when we modify 'a' (by using '+=' to append [4]), 'b' also reflects the change.





a: str  = "1, 2, 3"
b: str  = a
a += ", 4"


# After a += ", 4", a will become "1, 2, 3, 4", but b will remain "1, 2, 3"
# print("s:", s)  # Output: "1, 2, 3, 4"
# print("t:", t)  # Output: "1, 2, 3"

# Explanation: Strings in Python are immutable, so 's' and 't' do not refer to the same object.
# When 's' is modified, a new string is created, and 't' remains unchanged.











# More unexpected behaviour: 


x: int = 5
assert type(x) == int
assert isinstance(x, int), f"Expected int, got {type(x)}"


assert isinstance(True, bool)

assert isinstance(True, int)
# assert (type(True) is int)
# print(True + True + True)











# Mutable and Immutable Types in Python:
# Is int mutable or immutable? Let us find out!




# Here is a function that prints the memory address of a given object.
def show_memory(obj: object, name: str) -> None:
    print(f"\n{name}:")
    print(f"  Value: {obj}")
    print(f"  Type: {type(obj)}")
    print(f"  Memory address (id): {id(obj)}")

x: int = 42

# show_memory(x, "Original Integer")

x += 2
# let y = a new int, set y to x + 2
# rename y to x, and forget the old x

# show_memory(x, "Updated Integer")

z: int = 42
# show_memory(z, "My New Z")

big_str: str = "A" * 100
# show_memory(big_str, "Large string")

big_str: str = "A" * 101
# show_memory(big_str, "Large string")

l : List[int] = [1, 2, 3]
# show_memory(l, "Original List")

l.append(2)
# show_memory(l, "Updated List")

# show_memory(l[1], "Updated List")
# show_memory(l[3], "Updated List")

# Type Mismatch

a: int = 5
b: str = "hello"
result = b * a

# print (result)







# Lists in Python

# A Python list is a dynamic array that can store multiple data types.
my_list = [10, "hello", [3.14, True]]

# print(my_list)
# What is its type? Technically, this can be defined using Union types. But it is easy to see that such lists would not be useful when going for map, filter, or fold. 









# In Python, one can even do some really dangerous typing business:
self_ref_list = [1]
self_ref_list.append(self_ref_list)
# print(self_ref_list)  
# print (self_ref_list in self_ref_list)
# print (self_ref_list[1][1][1][1][1][1][1][1][1])



l = self_ref_list
while True:
     print(l[1])
     l = l[1]








# More Funny Business!

l = [1, "ueoe2", False, "hello", 5, 2.3]
for x in l:
    l.remove(x)
# print(l)
