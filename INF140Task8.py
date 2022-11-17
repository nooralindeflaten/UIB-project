
def computeN(p,q):
    n = q*p
    return n  

def computePhi(p,q):
    phiN = (p-1)*(q-1)
    return phiN

def gcd(x,y):
    if y == 0:
        return x
    return gcd(y,x%y)

def computeD(p,q,e):
    N = computePhi(p,q)
    d = 0
    while d < N:
        if (d*e)%N == 1:
            return d
        else:
            d += 1
    
def RSA(p,q,e,M):
    phiN = computePhi(p,q)
    N = computeN(p,q)
    d = computeD(p,q,e)
    Cipher = pow(M,e,N)
    Decryption = pow(Cipher,d,N)
    return Cipher, Decryption
    

def ACIIencode(Message):
    table = []
    for i in Message:
        table.append(ord(i))
    return table

def encryption(Message, p,q,e):
    N = computeN(p,q)
    Phi = computePhi(p,q)
    d = computeD(p,q,e)
    encryptionText = []
    decryptionText = []
    ac = ACIIencode(Message)
    for i in range(len(ac)):
        letter = RSA(p,q,e,ac[i])
        encryptionText.append(letter[0])
        decryptionText.append(letter[1])
    return encryptionText


def convert(numbers):
    b = []
    for item in numbers:
        b.append(hex(item))
    return b



def Dieffel(p,g,PUBa,PUBb):
    #PubA = x^Ka
    #PubB = x^Kb
    # to find alice's private value a, and 
    # alice = B^a mod n. 
    #Alice computes master = M = B^a mod n = g^ba
    # B^a mod n = A^b mod n
    a = 0
    A = 0
    while A != PUBa:
        A = pow(g,a,p)
        a += 1
    
    b = 0
    B = 0
    while B != PUBb:
        B = pow(g,b,p)
        b += 1
    
    print("Bob's private: ", b, " Bob's Public: ", B)
    print("Alice's private: ", a, " Alice's public ", A)
    
    K = pow(g,a*b,p)
    print("shared secret key (g**ab mod p) :", K)
    
    
def main():
    p = 5
    q = 17
    e = 7
    m = 5 
    
    
    word = "CRYPTOGRAPHY"
    print(RSA(p,q,e,m))
    E = encryption(word,13,19,7)
    print(convert(E))
    
    print(Dieffel(331,3,10,8))
if __name__=="__main__":
    main()