

'''
Task 4:

encryption of a toy cipher. 
ignoring the spaces, punctuations and capitalization in the original message
Vignere cipher encrypts the input message with secret K1
--> outputs Text 1
Text 1 is fed to column transportation cipher
with a secret key K2
--> outputs Text 2

Text 2 is then the output of round 1. 
Text 2 is the input and does the same again 
and returns the cipher text. 

example vigenere
attackatdawn
lemon

step 1:
make the length of the key match the length of the plaintext

attackatdawn
lemonlemonle
a = 1
so a = a -> (l steps which is = 12) L
t = 20
so t = t -> (e steps which is = 5) X
t = 20
so t = t -> (m steps which is = 13) F
a = 1
so a = a -> (o steps which is = )


vigenere cipher function 

1. ignore spaces, capitalization and punctuations 
2. length of the key to match the length of the text

3. find the steps the letter needs to be substituted
    so the letter from the plaintext's position in the alphabet 
    the position of the letter in the key
    = new letter. 
    a = 0
    l = 
'''



# 1: clean up string 

def cleanString(P):
    s = ""
    for ch in P:
        if ch.isalpha():
            s += ch
    return s.upper()



# make key length the string length. 
def keyGen(P,K):
    if len(P) == len(K):
        return K
    else:
        for i in range(len(P)-len(K)):
            K += K[i%len(K)]
    return K.upper()


# the key is now the right length and upper case, and the string is clean and uppercase
def vigenere(P,K1):
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    alphabet = alphabet.upper()
    #make alphabet uppercase.
    PLAIN = cleanString(P)
    #clean string and then key generate. 
    KEY = keyGen(PLAIN,K1)
    Cipher = ""
    for i in range(len(PLAIN)):
        #print(PLAIN, KEY)
        pos = alphabet.index(PLAIN[i])
        p2 = alphabet.index(KEY[i])
        Cipher += (alphabet[(pos+p2)%26])
    return Cipher


# output from vignere is now the input for the transposition.
# we need to generate key column 

#generate columns from the vignere
def ColumnGen(P,K2):
    P = cleanString(P)
    P = [x for x in P]
    words = []
    for i in range(0,len(P), len(K2)):
        words.append(P[i:i+len(K2)])
    return words
    

                   
         
# we now have a list of the plaintext and the key
# check the number from the key
# then all the numbers of row i  


def transposition(P,K2):
    ColumnOrder = ColumnGen(P,K2)
    CIPHERLIST = [None]*len(K2)
    for i in range(len(K2)):
        # number 6 = for j in range
        word = []
        for j in range(len(ColumnOrder)):
            if len(ColumnOrder[j]) > i:
                word.append(ColumnOrder[j][i])
        CIPHERLIST[(int(K2[i])-1)] = word
    return CIPHERLIST


def convert(P):
    p = ""
    for x in P:
        for y in x:
            p += str(y)
    return p     

def fixCipher(P,K2):
    P2 = transposition(P,K2)
    f = [x for i in P2 for x in i]
    words = []
    length = len(P)/len(K2)
    for i in range(0,len(f),length):
        words.append(f[i:i +length])
    return convert(words)
        
    # we need to have an i which 

'''
list of words/column
key length = 6
split text in to words of length 6
Nooraermittnavn
6
Noorae
words += ordet

[[Noorae], [rmittna], [vn]]



'''
def toycipher(P,K1,K2):
    #first send the plaintext through vignere with k1
    # all the steps are already done for this one
    i = 0
    while i < 2:
        text1 = vigenere(P,K1)
        print("output from vigenere cipher round ", i+1, " is: ", text1)
        text2 = fixCipher(text1,K2)
        #print("output after transposition encryption round ", i+1, " : ", text2)
        P = text2
        i += 1
    return P
'''
vignere.

'''


# Decryption:

def vigenereDecryption(C,K1):
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    alphabet = alphabet.upper()
    CIPHER = cleanString(C)
    plaintext = ""
    KEY = keyGen(CIPHER,K1)
    for i in range(len(CIPHER)):
        pos1 = alphabet.index(CIPHER[i])
        pos2 = alphabet.index(KEY[i])
        plaintext += (alphabet[((pos1-pos2)+26)%26])
    return plaintext
        

'''
word = 
evlna, cdtes

key i = 2
columngen = 
'''      

def decryptionkey(C,K2):
    C = cleanString(C)
    c = [x for x in C]
    #print(c)
    length = len(c)/len(K2)
    columns = []
    for i in range(0,len(c),length):
        word = c[i:i+length]
        columns.append(word)
    return columns


        #c.append(word)
def stringfix(C):
    s = ""
    for c in C:
        s += c   
    return s
         
def transDecryption(C,K2):
    #c = fixCipher(C,K2)
    #C = [x for x in C]
    # so if key = 6, i need the 6th column to be the 
    # first row,
    # for 
    #print(C)
    column = len(C)/len(K2)
    plain = ""
    col = 0
    while col < column:
        i = 0
        while i < len(K2):
            k = int(K2[i])-1
            l = k*column + col
            plain += C[l]
            i += 1
        i = 0
        col += 1
        if len(plain) == len(C):
            return plain
                #i += 1
    #print()
    return plain
    #return plain
            
        

    
    
         
def Decrypt(C,K1,K2):
    i = 0
    while i < 2:
        P1 = transDecryption(C,K2)
        print("decryption round", i+1, " transposition: ", P1)
        P2 = vigenereDecryption(P1,K1)
        print("vigenere decryption: ", i+1, P2)
        C = P2
        i += 1
    return C
        
    

def main():
    K1 = "cryptographyisfun"
    K2 = [3,4,1,5,7,2,6,8]
    P = "Cyber security is technologically complicated. It is a process not a product."
    
    c1 = "EVLNACDTESEAROFODEECWIREE"
    k2 = [6,3,2,4,1,5]
    C = toycipher(P,K1,K2)
    print("decrypt",Decrypt(C,K1,K2))
    print(toycipher(P,K1,K2))
    #print(cleanString(P))

if __name__=="__main__":
    main()