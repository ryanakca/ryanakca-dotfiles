import offlineimap
import re

prioritized = ['INBOX', 'QueensU.INBOX']

def mycmp(x, y):
  if offlineimap.__version__ < '6.4':
    return mycmp_oldofflineimap(x, y)
  else:
    return mycmp_newofflineimap(x, y)

def mycmp_oldofflineimap(x, y):
  for prefix in prioritized:
    xsw = x.startswith(prefix)
    ysw = y.startswith(prefix)
    if xsw and ysw:
      return cmp(x, y)
    elif xsw:
      return -1
    elif ysw:
      return +1
  return cmp(x, y)

def mycmp_newofflineimap(x, y):
  for prefix in prioritized:
    xsw = x.visiblename.startswith(prefix)
    ysw = y.visiblename.startswith(prefix)
    if xsw and ysw:
      return cmp(x, y)
    elif xsw:
      return -1
    elif ysw:
      return +1
  return cmp(x, y)
