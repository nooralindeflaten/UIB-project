
def cleanString(P):
    s = ""
    for ch in P:
        if ch.isalpha():
            s += ch
    return s.upper()

def blocks(message):
    Message = cleanString(message)
    block = []
    for i in range(0,len(Message),16):
        block.append(Message[i:i+16])
    
    for i in range(len(block)):
        while len(block[i]) < 16:
            block[i] += 'A'
    return block


def numberBlocks(message):
    block = blocks(message)
    numblocks = []
    b = []
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    alphabet = alphabet.upper()
    for i in range(len(block)):
        numblock = []
        for j in range(len(block[i])):
            letter = block[i][j]
            pos = alphabet.index(letter)
            numblock.append(pos)
        numblocks.append(numblock)
    
    for i in range(len(numblocks)):
        oneblock = []
        for j in range(0,len(numblocks[i]),4):
            oneblock.append(numblocks[i][j:j+4])
        b.append(oneblock)
    return b
            
def runningtotal(numBlock):
    total = []
    j = 0
    while j < len(numBlock[0]):
        k = 0
        sum = 0
        while k < len(numBlock):
            sum += numBlock[k][j]
            k += 1
        total.append(sum%26)
        j += 1
    return total
                
def rotation(numBlock):
    newBlock = []
    for i in range(len(numBlock)):
        blocklist = numBlock[i]
        b = blocklist[i+1:]+ blocklist[:i+1]
        if i == 3:
            b = list(reversed(blocklist))
        newBlock.append(b)
        
    #return the runningtotal of the new block
    print("new Block: ", newBlock)
    return runningtotal(newBlock)


def sumRunningTotal(total, newtotal):
    for i in range(len(total)):
        total[i] = (total[i]+newtotal[i])%26
    return total



def hash(message):
    NumBlocks = numberBlocks(message)
    hashmessage = []
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    alphabet = alphabet.upper()
    hashtotal = [0]*4
    
    for i in range(len(NumBlocks)):
        #i gives us the first 16 letter block.
        #get the runningtotal of the original block.
        Block1 = NumBlocks[i]
        originalRunningTotal = runningtotal(Block1)
        hashtotal = sumRunningTotal(hashtotal,originalRunningTotal)
        print("updated Value: ", i, hashtotal)
        # shift the block and add this runningtotal
        shiftedRunningTotal = rotation(Block1)
        print(shiftedRunningTotal)
        hashtotal = sumRunningTotal(hashtotal,shiftedRunningTotal)
        print(hashtotal)
    
    for i in range(len(hashtotal)):
        hashmessage.append(alphabet[hashtotal[i]])
    return hashmessage, hashtotal
                   
    #return AList         
    
    
    # we now have a list of the numbers and the guesses
    
    
     
# if the first numberbox = a +.. mod 26
# find the letter in the first pos by plussing until the last number is equal to 8
'''
    A B C D 
    E F G H
    I J K L
    so if 0 + 1 mod 26
    
    so 0 +24 
    runningtotal = 24+x mod 26 =
    so A
    sum of A mod sum of B = 8
    and 8 mod 26 = 8
    so sum of A + sum of B = 8
    
    
    I = 8, and 0+x mod 26 = 8
    so 8-0 + 26 mod 26
    0 + 
    i = 8
    og h = 7
    so 7+0 mod 26 = 8
    '''
    
def main():
    message = "He left twenty million US dollars to his beloved children"
    m1 = "AHJY VAAA AAAA AAAA AAAA AAAA AAAA AAAA AAAA AAAA AAAA AAAA"
    m2  = "I leave twenty million dollars to my friendly cousin Bill."
    block = blocks(message)
    print(block)
    g = numberBlocks(message)
    print(hash(m1))
    
if __name__=="__main__":
    main()
