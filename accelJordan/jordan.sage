#########################################################################################
#
# Reference: http://www.sagemath.org/doc/reference/sage/rings/qqbar.html
#            http://www.sagemath.org/doc/faq/faq-usage.html
#
# Compute the jordan form using the algebraic field to arbitrary precision??
#
# USAGE: ./sage -python ./jordan-sample.sage A1.a A1.out
#########################################################################################
from sage.all import *
import fractions as frac

infoPrints = False
printInterval = False

def error(msg):
  print 'error - '+msg
  sys.exit(1)

def warn(msg):
  print 'WARNING - '+msg,
  raw_input('[Press Enter to ack]')

# **********************************************************************
# 1. Parsing
# **********************************************************************

def readUnCommentedLine(fp):
  ## Skip lines with # as the FIRST character
  line = fp.readline()
  while(line[0] == '#'):
    line = fp.readline()
  if line == '' or line == '\n':
    error('readUnCommentedLine: file empty')
  return line

def get_A(fp):
  '''Takes in a float and constructs a rational!!'''
  def strToQQbar(strN):
    x = frac.Fraction(strN)
    return QQbar(x.numerator)/QQbar(x.denominator)
  ## read the line but remove the '\n'
  ## get the dimensions of the SQUARE matrix
  n = int(readUnCommentedLine(fp)[:-1])
  ## intialize an algebraic matrix
  matA = matrix(QQbar,n)
  ## read the elements of the matrix
  for i in range(n):
    ## read the line but remove the '\n'
    strA = readUnCommentedLine(fp)[:-1]
    #print strA
    ## TODO: decide delimiters
    strArr = strA.split(' ')
    numArr = map(strToQQbar,strArr)
    matA[i] = numArr
    #print matA
  return matA

# **********************************************************************
# 2. Jordan blocks
# **********************************************************************

def getJordanBlocks(JordanForm):
  ## Separate out the Jordan Blocks
  n,_ = JordanForm.dimensions()
  rowDiv,_ = JordanForm.subdivisions()
  numJB = len(rowDiv)+1
  #  print numJB,' Jordan Blocks found '
  JordanBlocks = []
  info = []
  i = 0
  j = 0
  for d in rowDiv:
    info.append((i,d-i))
    JordanBlocks.append(JordanForm.submatrix(i,i,d-i,d-i))
    i = d
  info.append((i,n-i))
  JordanBlocks.append(JordanForm.submatrix(i,i,n-i,n-i))
  return (JordanBlocks,info)

# **********************************************************************
# 3. Printing number and matrix
# **********************************************************************

#def AAToInterval(x,prec=53):
#  CIF64 = ComplexIntervalField(prec)
#  return (CIF64(x)).real().str(style='brackets',base=2)

#def AAToStr(x,prec=53):
#  CIF64 = ComplexIntervalField(prec)
#  return (CIF64(x)).real().str(base=10,style='question')

def dumpMat(fp,matM):
  global useIinterval
  r,c = matM.dimensions()
  if r!=c:
    error('dumpMat: dumps only a square matrix!')
  print >> fp,'[',
  for i in range(r):
    print >> fp,'[',
    for j in range(c):
      print >> fp,matM[i,j].real(),
      if j<c-1: print >> fp,',',
    print >> fp,']',
    if i<r-1: print >> fp, ',',
  print >> fp,']'

def dumpMyJordanForm(fp,myJordanForm):
  print >>fp, myJordanForm

def writeFile(fp,S,Sinv,myJordanForm):
    print >> fp, 'S'
    dumpMat(fp,S)
    print >> fp, 'Jordan'
    dumpMyJordanForm(fp,myJordanForm)
    print >> fp, 'Sinv'
    dumpMat(fp,Sinv)


# **********************************************************************
# 3. Real Jordan form
# **********************************************************************

def makeMatrixReal(m):
  r,c = m.dimensions()
  ans=True
  for i in range(r):
    for j in range(c):
      ans = ans and m[i,j].imag().is_zero()
      re = m[i,j].real()
      if re.is_one():
        m[i,j]=1
      elif re.is_zero():
        m[i,j]=0
      else:
        m[i,j] = re
  assert(ans)
  return ans


def popIdx(x,s,L):
  for i in range(len(L)):
    mat = L[i]
    if x == mat[0,0] and s == mat.dimensions()[0]:
      L.pop(i)
      return i
  return None

def getU(n,r):
  U = matrix(QQbar,2*n+r,2*n+r)
  j = 0
  for i in range(n):
    U[i,j] = 1
    U[i,j+1] = -I
    j += 2
  j = 0
  for i in range(n,2*n):
    U[i,j] = 1
    U[i,j+1] = I
    j += 2
  for i in range(2*n,2*n+r):
    U[i,i] = sqrt(2)
#  print 'U'
#  print U
  return U

'''get permutation matrix for a block diagonal matrix
nXn, [(diagonal index, size, new diagonal index)]'''
def getPermMatForBDMat(n,perm):
  pm = matrix(QQbar,n,n)
  ## new diagonal idx, old diagonal idx, size
  for ndi,odi,s in perm:
    ## Ones Matrix of size s
    #print s,s,1
    tmpMat = matrix(QQbar,s,s,1)
    pm[odi:odi+s,ndi:ndi+s] = tmpMat
  return pm



'''
definition:                             Sinv A S = J
permutation with Row and Col maatrices: R Sinv A S C = R J C
which are just inverses:                Pinv Sinv A S P = Pinv J P
real bases:                             U* Pinv Sinv A S P U = U* (J-perm) U
                                        ==> A = S P U U* (J-perm) U U* Pinv Sinv
                                        ==> A = [(S-permuted) U] [U* (J-perm) U] [U* (Sinv-permuted)]
                                        ==> A = S-real-permuted J-real-permuted Sinv-real-permuted
'''
def computeRealJordanForm(JordanForm):
  n,_ = JordanForm.dimensions()
  ## store all jordan blocks, but drop their conjugates
  JordanBlocks,JBIdx = getJordanBlocks(JordanForm)
  permutationScheme = []
  RealInfo = []
  RealEV = []
  CompInfo = []
  CompEV = []
  idx = 0
  while len(JordanBlocks) != 0:
    matJB = JordanBlocks[0]
    eigenvalue = matJB[0,0]
    ## if the eigenvalue is real
    if eigenvalue.imag().is_zero():
      JordanBlocks.pop(0)
      RealInfo.append(JBIdx.pop(0))
      RealEV.append((eigenvalue,matJB.dimensions()[0]))
    ## if the eigenvalue is complex
    else:
      JordanBlocks.pop(0)
      odi,s = JBIdx.pop(0)
      ## (current index, new index, size)
      oConjIdx,_ = JBIdx.pop(popIdx(eigenvalue.conjugate(),matJB.dimensions()[0],JordanBlocks))
      CompInfo.append((idx,odi,oConjIdx,s))
      CompEV.append((eigenvalue,matJB.dimensions()[0]))
      CompEV.append((eigenvalue.conjugate(),matJB.dimensions()[0]))
      idx += s
  JlSize = idx
  for ni,oi,ci,s in CompInfo:
    permutationScheme.append((ni,oi,s))
    permutationScheme.append((ni+JlSize,ci,s))
  Jsize = JlSize*2
  for i in range(len(RealInfo)):
    oi,s = RealInfo[i]
    ni = Jsize
    RealInfo[i] = (ni,oi,s)
    permutationScheme.append(RealInfo[i])
    Jsize += s

  LBlocks = []
  for eigenvalue,size in CompEV:
    LBlocks.append((eigenvalue,size))
  for eigenvalue,size in RealEV:
    LBlocks.append((eigenvalue,size))

  matPerm = getPermMatForBDMat(n,permutationScheme)
  permJordanForm = matPerm.inverse()*JordanForm*matPerm
  U = getU(JlSize,Jsize-JlSize*2)
  Ustar = U.conjugate_transpose()/QQbar(2)
  RealJordanForm = Ustar*permJordanForm*U
  subDivisionList = []
  for ni,_,_,_ in CompInfo:
    subDivisionList.append(ni*2)
  for ni,_,_ in RealInfo:
    subDivisionList.append(ni)
  ## do not use the first entry of (0,0)
  #print subDivisionList
  RealJordanForm.subdivide(subDivisionList[1:],subDivisionList[1:])

  return LBlocks,RealJordanForm,matPerm,U,Ustar

# **********************************************************************
# 3. Printing number and matrix
# **********************************************************************

def main():
  ## calling convention
  if len(sys.argv) < 3:
    error('usage:'+'sage -python '+sys.argv[0]+' <Matrix A input file> <Matrix J,S,S^-1 outpout file>')

  fileIn = sys.argv[1]
  if not(os.path.isfile(fileIn)):
    print 'File does not exist. Quitting.'
    return 1

  with open(fileIn, 'r') as fpIn:
    matA = get_A(fpIn)

    #  if infoPrints:
    ifp = sys.stdout
    #else:
    #ifp = open(os.devnull,'w')

  n,_ = matA.dimensions()
  #  print >> ifp, 'computing Jordan Form...'
  JordanForm,S = matA.jordan_form(subdivide=True,transformation=True)
  print >> ifp, 'JordanForm\n', JordanForm
  #  print >> ifp, 'computing S.inverse()...'
  Sinv = S.inverse()
  #  print >>ifp, 'S\n', S
  #  print >>ifp, 'Sinv\n', Sinv
  #  print >>ifp, 'A=SJS^-1\n', (S*JordanForm*Sinv)
  #  assert((S*JordanForm*Sinv-matA).is_zero())

  LBlocks,RealJordanForm,matPerm,U,Ustar = computeRealJordanForm(JordanForm);
  makeMatrixReal(RealJordanForm);
  print >>ifp, 'U\n', U
  print >>ifp, 'Ustar\n', Ustar
  print >>ifp, 'RealJordanForm\n', RealJordanForm
  realPermutedS = S*matPerm*U
  realPermutedSinv = Ustar*matPerm.inverse()*Sinv
  makeMatrixReal(realPermutedS);
  makeMatrixReal(realPermutedSinv);
  print >>ifp, 'realPermutedS\n',realPermutedS
  print >>ifp, 'realPermutedSinv\n',realPermutedSinv

  print >>ifp, 'LBlocks\n',LBlocks

  print >>ifp, 'A=realPermutedS.RealJ.realPermutedSinv\n', (realPermutedS*RealJordanForm*realPermutedSinv)

  print >> ifp, 'List of Jordan Blocks\n'
  myJordanForm = []
  lasteigenvalue = QQbar(-12345678987654321-98765432123456789*I)
  currentblock = []
  for (eigenvalue,size) in LBlocks:
    print >> ifp, 'eigenvalue: ', eigenvalue, ' size: ', size
    print >> ifp, '------------'
    if eigenvalue==lasteigenvalue:
      currentblock.append(size)
    else:
      myJordanForm.append((lasteigenvalue,currentblock))
      lasteigenvalue = eigenvalue
      currentblock = [size]
  myJordanForm.append((lasteigenvalue,currentblock))
  myJordanForm.pop(0)
  print >> ifp, 'result = \n', myJordanForm
  print >> ifp, 'writing output to a file...'
  fileOut = sys.argv[2]
  with open(fileOut, 'w') as fpOut:
    writeFile(fpOut,realPermutedS,realPermutedSinv,myJordanForm)

# Call main
main()
