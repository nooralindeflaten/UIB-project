
def euclid(a,b):
    if a == 0:
        return b, 0, 1
    gcd, u, v = euclid(b%a,a)
    x = v - (b // a) * u
    y = u
    return gcd, x, y

'''
a,b integer numbers and a =/ 0, 
one says a divides b if there is a k
such that b = ak


'''

def taskA(a,b):
    #b = b*1
    # if 1 divides b then there exicts a number k such that:
    # 1 = bk
    s = "a divides b: "
    print(s)
    r = a%b
    if r == 0:
        # 1 divides b:
        print( "true! a divides a, because k = 1 so a*k = a")
        k = (1/b)
        if k == 1:
            print("1 divides b because 1 = b*k", k)
        if a*0 == 0:
            s = s + "0 divides a because 0 = 0*a "
        
            
def isfactor(n,m):
    if n%m == 0:
        return True

def task2(a,b,m):
    # a congruent a mod n because n | a-a = a-a = n*some k 
    '''
    if a con b mod m, and b congruent c mod m then a congruent c mod m
    m | a-b = 
    a-b = m*k
    m | c-b = m*l
    c = m*l -b
    b = a / m*k
    
    n | a-c
    n | a-b
    n| b-c
    (a-b) = n*k
    a = n*k + b
    b = n*l + c
    a = n*k + n*l + c
    a-c = n(k+l)
    '''
           
            
    
    
def extendedeuclidean(a,b):
    r = a%b
    print(r)
    
    
def main():
    a = 35
    b = 5
    print(taskA(a,b))

if __name__=="__main__":
    main()
    