--
--  Options  
--

options.timeout = 120
options.subscribe = true



--  Accounts  


GMAIL = IMAP {
--    server = 'imap.gmail.com',
--    port = 993,
--    username = 'ryanakca@gmail.com',
    server = 'localhost',
    username = 'ryan',
    password = 'LOCAL_PASS',
}

-- Another account which connects to the mail server using the SSLv3

-- Get a list of the available mailboxes and folders
mailboxes, folders = GMAIL:list_all()

-- Get a list of the subscribed mailboxes and folders
mailboxes, folders = GMAIL:list_subscribed()

--
--   Filters   
--     For     
--    GMAIL    
--

-- Ubuntu stuff 
kubuntu = GMAIL.INBOX:contain_field('List-ID', 'kubuntu-devel.lists.ubuntu.com')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.kuDevel'], kubuntu)

ubuntuWeb = GMAIL.INBOX:contain_field('List-ID', 'ubuntu-website.lists.canonical.com')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.uWebML'], ubuntuWeb)

UbuntuDevelAnnounce = GMAIL.INBOX:contain_field('List-ID', 'ubuntu-devel-announce.lists.ubuntu.com')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.uDevelAnnounce'], UbuntuDevelAnnounce)


ubuntuCanada = GMAIL.INBOX:contain_field('List-ID', 'ubuntu-ca.lists.ubuntu.com')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.uCanada'], ubuntuCanada)

uDoc = GMAIL.INBOX:contain_field('List-ID', 'ubuntu-doc.lists.ubuntu.com')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.uDoc'], uDoc)

uClassroom = GMAIL.INBOX:contain_field('List-Id', 'ubuntu-classroom.lists.ubuntu.com')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.uClassroom'], uClassroom)

uClassroomOwner = GMAIL.INBOX:match_from('ubuntu-classroom-owner@lists.ubuntu.com')
GMAIL.INBOX:move_messages(GMAIL['[Google Mail].Bin'], uClassroomOwner)

launchpadUsers = GMAIL.INBOX:contain_field('List-ID', 'launchpad-users.lists.launchpad.net') +
                 GMAIL.INBOX:contain_field('List-ID', 'launchpad-users.lists.canonical.com') +
                 GMAIL.INBOX:match_to('launchpad-users@lists.launchpad.net') +
                 GMAIL.INBOX:match_cc('launchpad-users@lists.launchpad.net')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.launchpadUsers'], launchpadUsers)

kubuntuBugs = GMAIL.INBOX:contain_field('List-ID', 'kubuntu-bugs.lists.ubuntu.com') +
              GMAIL.INBOX:contain_header('X-Launchpad-Message-Rationale: .*@kubuntu-bugs') +
              GMAIL.INBOX:match_header('X-BeenThere: kubuntu-bugs@lists.ubuntu.com')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.kuBugs'], kubuntuBugs)

ubuntuirc = GMAIL.INBOX:contain_field('List-ID', 'ubuntu-irc.lists.ubuntu.com')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.ubuntu-irc'], ubuntuirc)

ubuntuinstaller = GMAIL.INBOX:contain_field('List-ID', 'ubuntu-installer.lists.ubuntu.com')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.ubuntu-installer'], ubuntuinstaller)

ubuntu = GMAIL.INBOX:contain_field('List-ID', '.*.lists.ubuntu.com')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu'], ubuntu)

uWebBugs = GMAIL.INBOX:match_header('X-Launchpad-Bug.*product=ubuntu-website.*')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.uWebBugs'], uWebBugs)

kuWebBugs = GMAIL.INBOX:match_header('X-Launchpad-Bug:.*product=kubuntu-website.*')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.kuWebBugs'], kuWebBugs)

ubugs =  GMAIL.INBOX:match_header('X-Launchpad-Bug:.*distribution=ubuntu;.*') +
         GMAIL.INBOX:contain_field('List-Id', 'ubuntu-bugcontrol.lists.launchpad.net')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.uBugs'], ubugs)

kubuntuninjas = GMAIL.INBOX:match_header('X-Launchpad-PPA: kubuntu-ninjas')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.kubuntu-ninjas'], kubuntuninjas)

--answers =  GMAIL.INBOX:match_header('X-Launchpad-Question: distribution=ubuntu.*')
--GMAIL.INBOX:mark_seen(answers)
--GMAIL.INBOX:move_messages(GMAIL['Ubuntu.uAnswers'], answers)

kubuntuWebmaster = GMAIL.INBOX:contain_to('webmaster@kubuntu.org')
GMAIL.INBOX:move_messages(GMAIL['Ubuntu.kuWebmaster'], kubuntuWebmaster)

-- Debian stuff 

debianmentorslist = GMAIL.INBOX:contain_field('List-ID', 'debian-mentors.lists.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.debian-mentors'], debianmentorslist)

debiandevel = GMAIL.INBOX:contain_field('List-ID', 'debian-devel.lists.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.debian-devel'], debiandevel)

debiandevelann = GMAIL.INBOX:contain_field('List-ID', 'debian-devel-announce.lists.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.debian-devel-announce'], debiandevelann)

debiannewmaint = GMAIL.INBOX:contain_field('List-ID', 'debian-newmaint.lists.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.debian-newmaint'], debiannewmaint)

debianpythonapps = GMAIL.INBOX:contain_field('List-ID', 'python-apps-team.lists.alioth.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.python-apps-team'], debianpythonapps)

debianqtkde = GMAIL.INBOX:contain_field('List-ID', 'debian-qt-kde.lists.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.debian-qt-kde'], debianqtkde)

debianpkgkdetalk = GMAIL.INBOX:contain_field('List-ID', 'pkg-kde-talk.lists.alioth.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.pkg-kde-talk'], debianpkgkdetalk)

debianpkgkdeextras = GMAIL.INBOX:contain_field('List-ID', 'pkg-kde-extras.lists.alioth.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.pkg-kde-extras'], debianpkgkdeextras)

debianpkgkdecommits = GMAIL.INBOX:contain_field('List-ID', 'pkg-kde-commits.lists.alioth.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.pkg-kde-commits'], debianpkgkdecommits)

debianpython = GMAIL.INBOX:contain_field('List-ID', 'debian-python.lists.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.debian-python'], debianpython)

debiandevelfrench = GMAIL.INBOX:contain_field('List-ID', 'debian-devel-french.lists.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.debian-devel-french'], debiandevelfrench)

debianpkgmultimediacommits = GMAIL.INBOX:contain_field('List-ID', 'pkg-multimedia-commits.lists.alioth.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.pkg-multimedia-commits'], debianpkgmultimediacommits)

debianpkgmultimediamaintainers = GMAIL.INBOX:contain_field('List-ID', 'pkg-multimedia-maintainers.lists.alioth.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.pkg-multimedia-maintainers'], debianpkgmultimediamaintainers)

debianjava = GMAIL.INBOX:contain_field('List-ID', 'debian-java.lists.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.debian-java'], debianjava)

debiannews = GMAIL.INBOX:contain_field('List-ID', 'debian-news.lists.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.debian-news'], debiannews)

debiandak = GMAIL.INBOX:contain_field('List-ID', 'debian-dak.lists.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.debian-dak'], debiandak)

debianproject = GMAIL.INBOX:contain_field('List-ID', 'debian-project.lists.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.debian-project'], debianproject)

debianqa = GMAIL.INBOX:contain_field('List-ID', 'debian-qa.lists.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.debian-qa'], debianqa)

dbugs = GMAIL.INBOX:match_from('.*@bugs.debian.org')
GMAIL.INBOX:move_messages(GMAIL['Debian.dBugs'], dbugs)

-- KDE Stuff

kdefrancophone = GMAIL.INBOX:contain_field('List-ID', 'kde-francophone.kde.org')
GMAIL.INBOX:move_messages(GMAIL['KDE.kde-francophone'], kdefrancophone)


-- Other computer stuff 
--
slashdot = ( GMAIL.INBOX:is_seen() * 
             GMAIL.INBOX:match_from('slashdot@slashdot.org') )
GMAIL.INBOX:move_messages(GMAIL['Slashdot'], slashdot)
--
lwn = ( GMAIL.INBOX:is_seen() * 
        GMAIL.INBOX:match_from('lwn@lwn.net') )
GMAIL.INBOX:move_messages(GMAIL['lwn'], lwn)

sbuild = GMAIL.INBOX:match_from('sbuild@.*.ryanak.ca') + 
        GMAIL.INBOX:match_from('sbuild@lambda')
GMAIL.INBOX:move_messages(GMAIL['sbuild'], sbuild)

freebsdstable = GMAIL.INBOX:contain_field('List-Id', 'freebsd-stable.freebsd.org')
GMAIL.INBOX:move_messages(GMAIL['FreeBSD.freebsd-stable'], freebsdstable)

freebsdcurrent = GMAIL.INBOX:contain_field('List-Id', 'freebsd-current.freebsd.org')
GMAIL.INBOX:move_messages(GMAIL['FreeBSD.freebsd-current'], freebsdcurrent)

-- O'ists

oactivists = GMAIL.INBOX:contain_field('List-Id', 'oactivists.googlegroups.com') *
             GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['OActivists'], oactivists)

oevolve = GMAIL.INBOX:contain_field('List-Id', 'oevolve.googlegroups.com') *
             GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['OEvolve'], oevolve)

oproducers = GMAIL.INBOX:contain_field('List-Id', 'oproducers.googlegroups.com') *
            GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['OProducers'], oproducers)

ogrownups = GMAIL.INBOX:contain_field('List-Id', 'ogrownups.googlegroups.com') *
            GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['OGrownups'], ogrownups)

ogeeks = GMAIL.INBOX:contain_field('List-ID', 'ogeeks.googlegroups.com') *
         GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['OGeeks'], ogeeks)

opeople = GMAIL.INBOX:contain_field('List-ID', 'opeople.googlegroups.com') *
         GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['OPeople'], opeople)

paleobloggers = GMAIL.INBOX:contain_field('List-Id', 'paleobloggers.googlegroups.com') *
            GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['PaleoBloggers'], paleobloggers)

paleocooks = GMAIL.INBOX:contain_field('List-Id', 'paleocooks.googlegroups.com') *
            GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['PaleoBloggers'], paleocooks)


-- Queen's

qsocial = ( GMAIL.INBOX:is_seen() *
        ( GMAIL.INBOX:match_to('social@cs.queensu.ca') +
          GMAIL.INBOX:match_cc('social@cs.queensu.ca') ) )
GMAIL.INBOX:move_messages(GMAIL['Queens.social'], qsocial)

qsail = ( GMAIL.INBOX:match_to('sail.*@cs.queensu.ca') +
          GMAIL.INBOX:match_cc('sail.*@cs.queensu.ca') +
          GMAIL.INBOX:match_to('sail4schedule@gmail.com') ) *
        GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['Queens.SAIL'], qsail)

-- People 

bagpipes = GMAIL.INBOX:match_from('PM_EMAIL') +
           GMAIL.INBOX:match_cc('PM_EMAIL')
GMAIL.INBOX:move_messages(GMAIL['Bagpipes'], bagpipes)

-- Misc

pjm = GMAIL.INBOX:match_from('webmaster@pajamasmedia.com') *
      GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['PJM'], pjm)

rubinreports = GMAIL.INBOX:match_from('profbarryrubin@yahoo.com') *
               GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['RubinReports'], rubinreports)

vfr = GMAIL.INBOX:match_from('vfr-no-reply@aynrand.org') *
      GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['VfR'], vfr)
