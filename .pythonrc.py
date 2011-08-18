# ~/.pythonrc
# enable syntax completion
try:
    import rlcompleter
    rlcompleter.readline.parse_and_bind("tab: complete")
except:
    import readline
    readline.parse_and_bind("tab: complete")
