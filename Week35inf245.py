
def euclid(a,b):
    if a == 0:
        return b, 0, 1
    gcd, u, v = euclid(b%a,a)
    x = v - (b // a) * u
    y = u
    return gcd, x, y


def binaryexponent(a,m,n):
    a %= n
    res = 1
    while m > 0:
        if m & 1:
            res = res * a % n
        a = a * a % n
        m >>= 1
    return res

def congruence(mat, n, xs):
    x = [None for _ in range(n)]
    for i in range(n-1, -1, -1):
        x[i] = mat[i][n]
        
        for j in range(i+1, n):
            x[i] -= mat[i][j]*x[j]
    
        x[i] = (x[i]/mat[i][i])
    print("soul: ")
    print(x[i])
'''
def eliminate(a,n):
    lead = 0
    rowCount = 5
    columnCount = 5
    for r in range(0,rowCount):
        if columnCount <= lead:
            break
        i = r
        while a[i,lead] == 0:
            i = i+1
            if rowCount == i:
                i = r
                lead = lead + 1
                if columnCount == lead:
                    break;
        if i != r:
            a[i], a[r] = a[r], a[i]
            
                
def rowSwap(mat, i, j, N):
    for k in range(N + 1):
        temp = mat[i][k]
        mat[i][k] = mat[j][k]
        mat[j][k] = temp  

def forwardElim(mat,N):
    for k in range(N):
        i_max = k
        v_max = mat[i_max][k]
        
        for i in range(k+1, N):
            if (abs(mat[i][k]) > v_max):
                v_max = mat[i][k]
                i_max = i
                
        if not mat[k][i_max]:
            return k 
        
        if(i_max != k):
            rowSwap(mat,k,i_max,N)
        
        for i in range(k + 1, N):
            f = mat[i][k]/mat[k][k]
            
            for j in range(k+1, N+1):
                mat[i][j] -= mat[k][j]*f
            mat[i][k] = 0
    
'''
def main():
    a = 135535135486417140
    b = 398828942339278511
    
    a2 = 393492946341
    m = 103587276991
    n = 444913350035
    
    matrice = [[1,-2,-2,-2,-1],[0,3,-2,-3,1],
               [3,0,0,1,-1], [3,-3,-2,0,1],[0,-3,3,-3,-3]]
    constants = [2,3,2,1,2]
    n2 = 456989977669
    
    print(euclid(a,b))
    print(binaryexponent(a2,m,n))

if __name__ == '__main__':
    main()