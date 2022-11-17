
def cleanString(P):
    s = ""
    for ch in P:
        if ch.isalpha():
            s += ch
    return s.upper()

def blocks(message):
    blocks = []
    m = cleanString(message)
    
    for i in range(0, len(m),4):
        blocks.append(m[i:i+4])
    
    for i in range(len(blocks)):
        if len(blocks[i]) < 4:
            blocks[i] += "A"
    return blocks

def matrix(message):
    block = blocks(message)
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    alphabet = alphabet.upper()
    Sblocks = ""
    CF = ""
    i = 0
    while i < len(block[0]):
        j = 0
        word = ""
        while j < len(block):
            Sblocks += block[j][i]
            #print(block[j][i])
            j += 1
        i += 1
        #Sblocks += word
        
    numberlist = []
    for i in range(0,len(Sblocks),len(block)):
        letter = Sblocks[i:i+len(block)]
        sum = 0
        for j in range(len(letter)):
            num = alphabet.find(letter[j])
            sum += num
        CF += alphabet[sum%26]
    return CF
            
        
        
        
            
            
        
            

'''
columnwise so:
AAAA
BBBB
CCCA

so 0+1+2
3 mod 26 = 3
'''
    

def main():
    text = "AAAABBBBCCC"
    print(blocks(text))
    print(matrix(text))


if __name__=="__main__":
    main()