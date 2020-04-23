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
      ssl = 'auto'
   }
end

--
--   Filters
--     For
--    GMAIL
--

mbrainz = ( GMAIL.INBOX:match_from('noreply@musicbrainz.org')
	  * GMAIL.INBOX:match_subject('Edits for your subscriptions')
	  * ( GMAIL.INBOX:is_older(1)
	    + GMAIL.INBOX:is_seen() ) )
GMAIL.INBOX:move_messages(GMAIL['MusicBrainz'], mbrainz)

-- Academia

typesann = GMAIL.INBOX:contain_field('List-Id', 'types-announce.lists.seas.upenn.edu')
GMAIL.INBOX:move_messages(GMAIL['Academia' .. folder_sep .. 'types-announce'], typesann)

typeslist = GMAIL.INBOX:contain_field('List-Id', 'types-list.lists.seas.upenn.edu') *
	    GMAIL.INBOX:is_seen()
GMAIL.INBOX:move_messages(GMAIL['Academia' .. folder_sep .. 'types-list'], typeslist)

categories = GMAIL.INBOX:contain_field('List-Id', 'maths-categories-seminar.lists.cam.ac.uk')
	     * ( GMAIL.INBOX:is_seen()
	       + GMAIL.INBOX:is_older(1) )
GMAIL.INBOX:move_messages(GMAIL['Academia'], categories)

fields = GMAIL.INBOX:match_from('.*@fields.utoronto.ca')
       * ( GMAIL.INBOX:is_old()
	 + GMAIL.INBOX:is_seen() )
fields = GMAIL.INBOX:move_messages(GMAIL['Academia'], fields)

splitwise = GMAIL.INBOX:match_from('hello@splitwise.com')
	    * ( GMAIL.INBOX:is_seen()
	      + GMAIL.INBOX:is_older(1) )
GMAIL.INBOX:move_messages(GMAIL['receipts'], splitwise)
