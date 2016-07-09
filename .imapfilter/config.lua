--
--  Options
--

options.timeout = 120
options.subscribe = true

filter_local = IMAPFILTER_LOCAL

--  Accounts

if filter_local then
   folder_sep = '.'
   GMAIL = IMAP {
      server = 'localhost',
      username = 'ryan',
      password = 'LOCAL_PASS',
   }
else
   folder_sep = '/'
   GMAIL = IMAP {
      server = 'imap.gmail.com',
      port = 993,
      username = 'ryanakca@gmail.com',
      password = 'GMAIL_PASS',
      ssl = 'tls1',
   }
end

--
--   Filters
--     For  
--    GMAIL 
--

-- Ubuntu stuff

ubuntu = { 'kubuntu-devel'
	   , 'ubuntu-ca'
	   , 'ubuntu-classroom'
	   , 'ubuntu-devel-announce'
	   , 'ubuntu-doc'
	   , 'ubuntu-installer'
	   , 'ubuntu-irc'
	   --, 'kubuntu-bugs'
}

canonical = { 'ubuntu-website' }


for list = 1, #ubuntu do
    listfilter = GMAIL.INBOX:contain_field('List-ID', ubuntu[list] .. '.lists.ubuntu.com')
    GMAIL.INBOX:move_messages(GMAIL['Ubuntu' .. '' .. folder_sep .. '' .. ubuntu[list]], listfilter)
end

for list = 1, #canonical do
   listfilter = GMAIL.INBOX:contain_field('List-ID', canonical[list] .. '.lists.canonical.com')
   GMAIL.INBOX:move_messages(GMAIL['Ubuntu' .. '' .. folder_sep .. '' .. canonical[list]], listfilter)
end

launchpadUsers = GMAIL.INBOX:contain_field('List-ID', 'launchpad-users.lists.launchpad.net') +
                 GMAIL.INBOX:contain_field('List-ID', 'launchpad-users.lists.canonical.com') +
                 GMAIL.INBOX:match_to('launchpad-users@lists.launchpad.net') +
                 GMAIL.INBOX:match_cc('launchpad-users@lists.launchpad.net')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu' .. folder_sep .. 'launchpadUsers'], launchpadUsers)

-- kubuntuBugs = GMAIL.INBOX:contain_header('X-Launchpad-Message-Rationale: .*@kubuntu-bugs') +
--               GMAIL.INBOX:match_header('X-BeenThere: kubuntu-bugs@lists.ubuntu.com')
-- GMAIL.INBOX:move_messages(GMAIL['Ubuntu' .. folder_sep .. 'kubuntu-bugs'], kubuntuBugs)

ubuntu = GMAIL.INBOX:contain_field('List-ID', '.*.lists.ubuntu.com')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu'], ubuntu)

uWebBugs = GMAIL.INBOX:match_header('X-Launchpad-Bug.*product=ubuntu-website.*')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu' .. folder_sep .. 'ubuntu-website-bugs'], uWebBugs)

kuWebBugs = GMAIL.INBOX:match_header('X-Launchpad-Bug:.*product=kubuntu-website.*')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu' .. folder_sep .. 'kubuntu-website-bugs'], kuWebBugs)

ubugs =  GMAIL.INBOX:match_header('X-Launchpad-Bug:.*distribution=ubuntu;.*') +
         GMAIL.INBOX:contain_field('List-Id', 'ubuntu-bugcontrol.lists.launchpad.net')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu' .. folder_sep .. 'ubuntu-bugs'], ubugs)

kubuntuninjas = GMAIL.INBOX:match_header('X-Launchpad-PPA: kubuntu-ninjas') +
                GMAIL.INBOX:match_header('X-Launchpad-PPA: kubuntu-ppa-staging') +
                GMAIL.INBOX:contain_field('List-ID', 'kubuntu-ppa.lists.launchpad.net')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu' .. folder_sep .. 'kubuntu-ninjas'], kubuntuninjas)

--answers =  GMAIL.INBOX:match_header('X-Launchpad-Question: distribution=ubuntu.*')
--GMAIL.INBOX:mark_seen(answers)
--GMAIL.INBOX:move_messages(GMAIL['Ubuntu' .. folder_sep .. 'uAnswers'], answers)

--kubuntuWebmaster = GMAIL.INBOX:contain_to('webmaster@kubuntu.org')
--GMAIL.INBOX:move_messages(GMAIL['Ubuntu' .. folder_sep .. 'kubuntu-webmaster'], kubuntuWebmaster)

-- Debian stuff

debiandevelann = GMAIL.INBOX:contain_field('List-ID', 'debian-devel-announce.lists.debian.org') *
   GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['Debian' .. folder_sep .. 'debian-devel-announce'], debiandevelann)

dbugs = GMAIL.INBOX:match_from('.*@bugs.debian.org') *
   GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['Debian' .. folder_sep .. 'dBugs'], dbugs)

listsdebianorg = { 'debian-backports'
		   , 'debian-bsd'
		   , 'debian-dak'
		   , 'debian-devel'
		   , 'debian-devel-french'
		   , 'debian-haskell'
		   , 'debian-java'
		   , 'debian-mentors'
		   , 'debian-newmaint'
		   , 'debian-news'
		   , 'debian-private'
		   , 'debian-project'
		   , 'debian-python'
		   , 'debian-qa'
		   , 'debian-qt-kde'
}

for list = 1, #listsdebianorg do
    listfilter = GMAIL.INBOX:contain_field('List-ID', listsdebianorg[list] .. '.lists.debian.org')
    GMAIL.INBOX:move_messages(GMAIL['Debian' .. '' .. folder_sep .. '' .. listsdebianorg[list]], listfilter)
end

alioth = { 'pkg-kde-commits'
	   , 'pkg-kde-extras'
	   , 'pkg-kde-talk'
	   , 'pkg-multimedia-commits'
	   , 'pkg-multimedia-maintainers'
	   , 'python-apps-team' }

for list = 1, #alioth do
   listfilter = GMAIL.INBOX:contain_field('List-ID', alioth[list] .. '.lists.alioth.debian.org')
   GMAIL.INBOX:move_messages(GMAIL['Debian' .. '' .. folder_sep .. '' .. alioth[list]], listfilter)
end

-- KDE Stuff

kdefrancophone = GMAIL.INBOX:contain_field('List-ID', 'kde-francophone.kde.org')
GMAIL.INBOX:move_messages(GMAIL['KDE' .. folder_sep .. 'kde-francophone'], kdefrancophone)

-- OpenBSD Stuff

openbsd = { 'announce'
	    , 'misc'
	    , 'mirrors-discuss'
	    , 'tech' }

for list = 1, #openbsd do
   listfilter = GMAIL.INBOX:contain_field('List-ID', openbsd[list] .. '.openbsd.org')
   GMAIL.INBOX:move_messages(GMAIL['OpenBSD' .. '' .. folder_sep .. '' .. openbsd[list]], listfilter)
end

sshud = GMAIL.INBOX:contain_field('List-ID', 'openssh-unix-dev.mindrot.org')
GMAIL.INBOX:move_messages(GMAIL['OpenBSD' .. folder_sep .. 'ssh-unix-dev'], sshud)

-- Other computer stuff
--
slashdot = ( GMAIL.INBOX:is_seen() *
             GMAIL.INBOX:match_from('slashdot@newsletters.slashdot.org') )
GMAIL.INBOX:move_messages(GMAIL['Slashdot'], slashdot)
--
lwn = ( GMAIL.INBOX:is_seen() *
        GMAIL.INBOX:match_from('lwn.*@lwn.net') )
GMAIL.INBOX:move_messages(GMAIL['lwn'], lwn)

sbuild = GMAIL.INBOX:match_from('sbuild@.*.ryanak.ca') +
        GMAIL.INBOX:match_from('sbuild@lambda')
GMAIL.INBOX:move_messages(GMAIL['sbuild'], sbuild)

frescobaldi = ( GMAIL.INBOX:is_seen() *
                GMAIL.INBOX:contain_field('List-ID', 'frescobaldi.googlegroups.com') )
GMAIL.INBOX:move_messages(GMAIL['Debian' .. folder_sep .. 'frescobaldi'], frescobaldi)

opensmtpd = ( GMAIL.INBOX:is_seen()
            + GMAIL.INBOX:is_older(2) ) *
                GMAIL.INBOX:contain_field('List-ID', 'misc.opensmtpd.org')
GMAIL.INBOX:move_messages(GMAIL['Debian' .. folder_sep .. 'opensmtpd'], opensmtpd)

-- O'ists

PiA = GMAIL.INBOX:match_from('.*@philosophyinaction.com') *
      GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['PiA'], PiA)

-- Queen's

qsocial = GMAIL.INBOX:match_to('social@.*cs.queensu.ca') +
          GMAIL.INBOX:match_cc('social@.*cs.queensu.ca')
GMAIL.INBOX:move_messages(GMAIL['Queens' .. folder_sep .. 'social'], qsocial)

qsail = ( GMAIL.INBOX:match_to('sail.*@cs.queensu.ca') +
          GMAIL.INBOX:match_cc('sail.*@cs.queensu.ca') ) *
        GMAIL.INBOX:is_seen() +
        GMAIL.INBOX:match_to('sail4schedule@gmail.com')
GMAIL.INBOX:move_messages(GMAIL['Queens' .. folder_sep .. 'SAIL'], qsail)

-- McGill

belcom = GMAIL.INBOX:match_to('beluga-commit@cs.mcgill.ca')
GMAIL.INBOX:move_messages(GMAIL['Internship' .. folder_sep .. 'beluga-commit'], belcom)

beldev = GMAIL.INBOX:match_to('beluga-dev@cs.mcgill.ca')
GMAIL.INBOX:move_messages(GMAIL['Internship' .. folder_sep .. 'beluga-dev'], beldev)

complogic = GMAIL.INBOX:contain_field('List-Id', 'complogic.CS.McGill.CA') *
            GMAIL.INBOX:is_older(1)
GMAIL.INBOX:move_messages(GMAIL['Internship' .. folder_sep .. 'complogic'], complogic)

-- MIT under GMAIL

mitplv = ( GMAIL.INBOX:contain_field('List-Id', 'plv.csail.mit.edu')
         + GMAIL.INBOX:contain_field('List-Id', 'bedrock-group.lists.csail.mit.edu') ) *
         GMAIL.INBOX:is_older(2)
GMAIL.INBOX:move_messages(GMAIL['GMAIL' .. folder_sep .. 'MIT'], mitplv)

-- Academia

typesann = GMAIL.INBOX:contain_field('List-Id', 'types-announce.lists.seas.upenn.edu')
GMAIL.INBOX:move_messages(GMAIL['Academia' .. folder_sep .. 'types-announce'], typesann)

typeslist = GMAIL.INBOX:contain_field('List-Id', 'types-list.lists.seas.upenn.edu') *
            GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['Academia' .. folder_sep .. 'types-list'], typeslist)

-- People

bagpipes = GMAIL.INBOX:match_from('PM_EMAIL') +
           GMAIL.INBOX:match_cc('PM_EMAIL')
GMAIL.INBOX:move_messages(GMAIL['Bagpipes'], bagpipes)

facebook = GMAIL.INBOX:match_from('.*@facebookmail.com') *
         ( GMAIL.INBOX:contain_subject('New messages from ') +
           GMAIL.INBOX:contain_subject('New message from ') ) *
         ( GMAIL.INBOX:is_old()
         + GMAIL.INBOX:is_seen() )
GMAIL.INBOX:move_messages(GMAIL['Friends'], facebook)

-- Misc

vfr = GMAIL.INBOX:match_from('vfr-no-reply@aynrand.org') *
      GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['VfR'], vfr)

epic = GMAIL.INBOX:contain_field('List-Id', 'Every Pub In Cambridge <epic.einval.com>')
GMAIL.INBOX:move_messages(GMAIL['Internship' .. folder_sep .. 'epic'], epic)
