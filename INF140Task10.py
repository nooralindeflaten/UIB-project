
import itertools


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
def vigenereDecryption(C,K1):
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    alphabet = alphabet.upper()
    CIPHER = cleanString(C)
    plaintext = ""
    KEY = keyGen(CIPHER,K1)
    #print(CIPHER)
    #print(KEY)
    for i in range(len(CIPHER)):
        pos1 = alphabet.index(CIPHER[i])
        pos2 = alphabet.index(KEY[i])
        plaintext += (alphabet[((pos1-pos2)+26)%26])
    return plaintext

'''
choose (p,q) = (103,113), e = 5
M = some form of Key


Encrypt the public key (n,e) = (116389, 5)
'''
def keyToString(key):
    s = ''
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    alphabet = alphabet.upper()
    for i in range(len(key)):
        letter = alphabet.index(key[i])
        s += str(letter)
    return int(s)
    

def convertKey(number):
    k1 = [int(x) for x in str(number)]
    lookupTable = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J']
    s = ''
    for i in k1:
        s += lookupTable[i]
    return s.upper()

def FunCipher(p,q,e,Message,Number,Keys):
    # he sends the cipher text... and the number.
    #first make number keys to a joined string, and then an int
    # Let's try all the keys and see which one gives the correct number. 
    n = p*q
    k = 0
    for i in range(len(Keys)):
        Key = keyToString(Keys[i])
        B = pow(Key,e,n)
        if B == Number:
            k += Key
    
    finalKey = convertKey(k)
    print(finalKey)
    decryption = vigenereDecryption(Message,finalKey)
    return decryption
            
            
        
def main():
    KEY = ['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J']
    keys = [''.join(i) for i in itertools.product(KEY,repeat=4)]
    print(len(keys))
    
    TobiasMessage = "tsaatmvttlezlwbhvbproaszletpdmaliaxplvmhvmvrtfxevhqmjmyivaxgwiavjkvqxhrqlwquxplawyplalwziboeboedlfmlrjyiijlmkevkovvaqaevkxpvwmalialicijlivivmhgplhiuhlvrwaovvaqa."
    TB = cleanString(TobiasMessage)
    Number = 10998
    
    print(FunCipher(103,113,5,TB,Number,keys))
    
    
if __name__=="__main__":
    main()