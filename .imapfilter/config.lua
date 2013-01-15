--
--  Options
--

options.timeout = 120
options.subscribe = true



--  Accounts

REMOTE_GMAIL = IMAP {
    server = 'imap.gmail.com',
    port = 993,
    username = 'ryanakca@gmail.com',
    password = 'GMAIL_PASS',
    ssl = 'tls1',
}

LOCAL_GMAIL = IMAP {
    server = 'localhost',
    username = 'ryan',
    password = 'LOCAL_PASS',
}

-- Another account which connects to the mail server using the SSLv3

-- Get a list of the available mailboxes and folders
mailboxes, folders = IMAPFILTER_GMAIL_SERVER:list_all()

-- Get a list of the subscribed mailboxes and folders
mailboxes, folders = IMAPFILTER_GMAIL_SERVER:list_subscribed()

--
--   Filters
--     For  
--    GMAIL 
--

-- Ubuntu stuff
kubuntu = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'kubuntu-devel.lists.ubuntu.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.kuDevel'], kubuntu)

ubuntuWeb = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'ubuntu-website.lists.canonical.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.uWebML'], ubuntuWeb)

UbuntuDevelAnnounce = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'ubuntu-devel-announce.lists.ubuntu.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.uDevelAnnounce'], UbuntuDevelAnnounce)

ubuntuCanada = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'ubuntu-ca.lists.ubuntu.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.uCanada'], ubuntuCanada)

uDoc = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'ubuntu-doc.lists.ubuntu.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.uDoc'], uDoc)

uClassroom = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-Id', 'ubuntu-classroom.lists.ubuntu.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.uClassroom'], uClassroom)

uClassroomOwner = IMAPFILTER_GMAIL_SERVER.INBOX:match_from('ubuntu-classroom-owner@lists.ubuntu.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['[Google Mail].Bin'], uClassroomOwner)

launchpadUsers = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'launchpad-users.lists.launchpad.net') +
                 IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'launchpad-users.lists.canonical.com') +
                 IMAPFILTER_GMAIL_SERVER.INBOX:match_to('launchpad-users@lists.launchpad.net') +
                 IMAPFILTER_GMAIL_SERVER.INBOX:match_cc('launchpad-users@lists.launchpad.net')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.launchpadUsers'], launchpadUsers)

kubuntuBugs = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'kubuntu-bugs.lists.ubuntu.com') +
              IMAPFILTER_GMAIL_SERVER.INBOX:contain_header('X-Launchpad-Message-Rationale: .*@kubuntu-bugs') +
              IMAPFILTER_GMAIL_SERVER.INBOX:match_header('X-BeenThere: kubuntu-bugs@lists.ubuntu.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.kuBugs'], kubuntuBugs)

ubuntuirc = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'ubuntu-irc.lists.ubuntu.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.ubuntu-irc'], ubuntuirc)

ubuntuinstaller = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'ubuntu-installer.lists.ubuntu.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.ubuntu-installer'], ubuntuinstaller)

ubuntu = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', '.*.lists.ubuntu.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu'], ubuntu)

uWebBugs = IMAPFILTER_GMAIL_SERVER.INBOX:match_header('X-Launchpad-Bug.*product=ubuntu-website.*')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.uWebBugs'], uWebBugs)

kuWebBugs = IMAPFILTER_GMAIL_SERVER.INBOX:match_header('X-Launchpad-Bug:.*product=kubuntu-website.*')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.kuWebBugs'], kuWebBugs)

ubugs =  IMAPFILTER_GMAIL_SERVER.INBOX:match_header('X-Launchpad-Bug:.*distribution=ubuntu;.*') +
         IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-Id', 'ubuntu-bugcontrol.lists.launchpad.net')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.uBugs'], ubugs)

kubuntuninjas = IMAPFILTER_GMAIL_SERVER.INBOX:match_header('X-Launchpad-PPA: kubuntu-ninjas') +
                IMAPFILTER_GMAIL_SERVER.INBOX:match_header('X-Launchpad-PPA: kubuntu-ppa-staging') +
                IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'kubuntu-ppa.lists.launchpad.net')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.kubuntu-ninjas'], kubuntuninjas)

--answers =  IMAPFILTER_GMAIL_SERVER.INBOX:match_header('X-Launchpad-Question: distribution=ubuntu.*')
--IMAPFILTER_GMAIL_SERVER.INBOX:mark_seen(answers)
--IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.uAnswers'], answers)

kubuntuWebmaster = IMAPFILTER_GMAIL_SERVER.INBOX:contain_to('webmaster@kubuntu.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Ubuntu.kuWebmaster'], kubuntuWebmaster)

-- Debian stuff

debianmentorslist = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-mentors.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-mentors'], debianmentorslist)

debiandevel = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-devel.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-devel'], debiandevel)

debiandevelann = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-devel-announce.lists.debian.org') *
                 IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-devel-announce'], debiandevelann)

debiannewmaint = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-newmaint.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-newmaint'], debiannewmaint)

debianpythonapps = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'python-apps-team.lists.alioth.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.python-apps-team'], debianpythonapps)

debianqtkde = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-qt-kde.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-qt-kde'], debianqtkde)

debianpkgkdetalk = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'pkg-kde-talk.lists.alioth.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.pkg-kde-talk'], debianpkgkdetalk)

debianpkgkdeextras = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'pkg-kde-extras.lists.alioth.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.pkg-kde-extras'], debianpkgkdeextras)

debianpkgkdecommits = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'pkg-kde-commits.lists.alioth.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.pkg-kde-commits'], debianpkgkdecommits)

debianpython = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-python.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-python'], debianpython)

debiandevelfrench = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-devel-french.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-devel-french'], debiandevelfrench)

debianpkgmultimediacommits = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'pkg-multimedia-commits.lists.alioth.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.pkg-multimedia-commits'], debianpkgmultimediacommits)

debianpkgmultimediamaintainers = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'pkg-multimedia-maintainers.lists.alioth.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.pkg-multimedia-maintainers'], debianpkgmultimediamaintainers)

debianjava = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-java.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-java'], debianjava)

debiannews = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-news.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-news'], debiannews)

debiandak = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-dak.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-dak'], debiandak)

debianproject = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-project.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-project'], debianproject)

debianqa = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-qa.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-qa'], debianqa)

debianprivate = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-private.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-private'], debianprivate)

debianhaskell = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'debian-haskell.lists.debian.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.debian-haskell'], debianhaskell)

dbugs = IMAPFILTER_GMAIL_SERVER.INBOX:match_from('.*@bugs.debian.org') *
        IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.dBugs'], dbugs)

-- KDE Stuff

kdefrancophone = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'kde-francophone.kde.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['KDE.kde-francophone'], kdefrancophone)


-- Other computer stuff
--
slashdot = ( IMAPFILTER_GMAIL_SERVER.INBOX:is_seen() *
             IMAPFILTER_GMAIL_SERVER.INBOX:match_from('slashdot@newsletters.slashdot.org') )
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Slashdot'], slashdot)
--
lwn = ( IMAPFILTER_GMAIL_SERVER.INBOX:is_seen() *
        IMAPFILTER_GMAIL_SERVER.INBOX:match_from('lwn@lwn.net') )
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['lwn'], lwn)

sbuild = IMAPFILTER_GMAIL_SERVER.INBOX:match_from('sbuild@.*.ryanak.ca') +
        IMAPFILTER_GMAIL_SERVER.INBOX:match_from('sbuild@lambda')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['sbuild'], sbuild)

freebsdstable = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-Id', 'freebsd-stable.freebsd.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['FreeBSD.freebsd-stable'], freebsdstable)

freebsdcurrent = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-Id', 'freebsd-current.freebsd.org')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['FreeBSD.freebsd-current'], freebsdcurrent)

frescobaldi = ( IMAPFILTER_GMAIL_SERVER.INBOX:is_seen() *
                IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'frescobaldi.googlegroups.com') )
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Debian.frescobaldi'], frescobaldi)

-- O'ists

oactivists = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-Id', 'oactivists.googlegroups.com') *
             IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['OActivists'], oactivists)

oevolve = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-Id', 'oevolve.googlegroups.com') *
             IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['OEvolve'], oevolve)

oproducers = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-Id', 'oproducers.googlegroups.com') *
            IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['OProducers'], oproducers)

ogrownups = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-Id', 'ogrownups.googlegroups.com') *
            IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['OGrownups'], ogrownups)

ogeeks = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'ogeeks.googlegroups.com') *
         IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['OGeeks'], ogeeks)

opeople = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-ID', 'opeople.googlegroups.com') *
         IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['OPeople'], opeople)

paleobloggers = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-Id', 'paleobloggers.googlegroups.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['PaleoBloggers'], paleobloggers)

paleocooks = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-Id', 'paleocooks.googlegroups.com') *
            IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['PaleoBloggers'], paleocooks)

PiA = IMAPFILTER_GMAIL_SERVER.INBOX:match_from('.*@philosophyinaction.com') *
      IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['PiA'], PiA)

-- Queen's

qsocial = ( IMAPFILTER_GMAIL_SERVER.INBOX:is_seen() *
        ( IMAPFILTER_GMAIL_SERVER.INBOX:match_to('social@cs.queensu.ca') +
          IMAPFILTER_GMAIL_SERVER.INBOX:match_cc('social@cs.queensu.ca') ) )
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Queens.social'], qsocial)

qsail = ( IMAPFILTER_GMAIL_SERVER.INBOX:match_to('sail.*@cs.queensu.ca') +
          IMAPFILTER_GMAIL_SERVER.INBOX:match_cc('sail.*@cs.queensu.ca') ) *
        IMAPFILTER_GMAIL_SERVER.INBOX:is_seen() +
        IMAPFILTER_GMAIL_SERVER.INBOX:match_to('sail4schedule@gmail.com')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Queens.SAIL'], qsail)

-- McGill

belcom = IMAPFILTER_GMAIL_SERVER.INBOX:match_to('beluga-commit@cs.mcgill.ca')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Internship.beluga-commit'], belcom)

beldev = IMAPFILTER_GMAIL_SERVER.INBOX:match_to('beluga-dev@cs.mcgill.ca')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Internship.beluga-dev'], beldev)

-- Academia

typesann = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-Id', 'types-announce.lists.seas.upenn.edu')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Academia.types-announce'], typesann)

-- People

bagpipes = IMAPFILTER_GMAIL_SERVER.INBOX:match_from('PM_EMAIL') +
           IMAPFILTER_GMAIL_SERVER.INBOX:match_cc('PM_EMAIL')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Bagpipes'], bagpipes)

-- Misc

pjm = IMAPFILTER_GMAIL_SERVER.INBOX:match_from('webmaster@pjmedia.com') *
      IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['PJM'], pjm)

rubinreports = IMAPFILTER_GMAIL_SERVER.INBOX:match_from('profbarryrubin@yahoo.com') *
               IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['RubinReports'], rubinreports)

vfr = IMAPFILTER_GMAIL_SERVER.INBOX:match_from('vfr-no-reply@aynrand.org') *
      IMAPFILTER_GMAIL_SERVER.INBOX:is_seen()
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['VfR'], vfr)

epic = IMAPFILTER_GMAIL_SERVER.INBOX:contain_field('List-Id', 'Every Pub In Cambridge <epic.einval.com>')
IMAPFILTER_GMAIL_SERVER.INBOX:move_messages(IMAPFILTER_GMAIL_SERVER['Internship.epic'], epic)
