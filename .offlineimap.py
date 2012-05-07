import re

prioritized = ['INBOX', 'QueensU.INBOX']

def mycmp(x, y):
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
