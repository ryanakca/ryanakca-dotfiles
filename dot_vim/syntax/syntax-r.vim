" Vim syntax file
" Language:	R (GNU S)
" Maintainer:	Tom Payne <tom@tompayne.org>
" Last Change:  2003 May 11
" Filenames:	*.r
" URL:		http://www.tompayne.org/vim/syntax/r.vim
"
" Modified by Jeremy Stephens and Will Gray.
"
" Options:
"       You can set these options in your vimrc file.
"
"       For set option do: let OPTION_NAME = 1
"       For clear option do: let OPTION_NAME = 0
"
" For highlighting internal/primitive functions:
"       r_package_internal
"
" For highlighting package 'base' functions:
"       r_package_base
"
" For highlighting package 'graphics' functions:
"       r_package_graphics
"
" For highlighting package 'stats' functions:
"       r_package_stats
"
" For highlighting package 'tools' functions:
"       r_package_tools
"
" For highlighting package 'methods' functions:
"       r_package_methods
"
" For highlighting package 'utils' functions:
"       r_package_utils
"
" If you want all possible R package highlighting:
"       r_package_all

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

if version >= 600
  setlocal iskeyword=@,48-57,_,.
else
  set iskeyword=@,48-57,_,.
endif

if !exists("r_package_all")
  let r_package_all = 1
endif
if exists("r_package_all") && r_package_all != 0
  if !exists("r_package_internal")
    let r_package_internal = 1
  endif
  if !exists("r_package_base")
    let r_package_base = 1
  endif
  if !exists("r_package_methods")
    let r_package_methods = 1
  endif
  if !exists("r_package_graphics")
    let r_package_graphics = 1
  endif
  if !exists("r_package_stats")
    let r_package_stats = 1
  endif
  if !exists("r_package_utils")
    let r_package_utils = 1
  endif
  if !exists("r_package_tools")
    let r_package_tools = 1
  endif
endif

syn case match

" Comment
syn match rComment /\#.*/

" Constant
" string enclosed in double quotes
syn region rString matchgroup=rStringDelimiter start=/"/ skip=/\\\\\|\\"/ end=/"/
" string enclosed in single quotes
syn region rString matchgroup=rStringDelimiter start=/'/ skip=/\\\\\|\\'/ end=/'/
" number with no fractional part or exponent
syn match rNumber /\d\+/
" floating point number with integer and fractional parts and optional exponent
syn match rFloat /\d\+\.\d*\([Ee][-+]\=\d\+\)\=/
" floating point number with no integer part and optional exponent
syn match rFloat /\.\d\+\([Ee][-+]\=\d\+\)\=/
" floating point number with no fractional part and optional exponent
syn match rFloat /\d\+[Ee][-+]\=\d\+/

" Identifier
" identifier with leading letter and optional following keyword characters
syn match rIdentifier /\a\k*/
" identifier with leading period, one or more digits, and at least one non-digit keyword character
syn match rIdentifier /\.\d*\K\k*/

syn match rFunction /\a\k*(/me=e-1 nextgroup=rParenRegion

" Statement
syn keyword rStatement   break next return
syn keyword rConditional if else
syn keyword rRepeat      for in repeat while

" Constant
syn keyword rConstant LETTERS letters month.ab month.name pi
syn keyword rConstant NULL
syn keyword rBoolean  FALSE TRUE
syn keyword rNumber   NA

syn match rArrow /<\{1,2}-/
syn match rArrow /->\{1,2}/

" Type
syn keyword rType array category character complex double function integer list logical matrix numeric vector data.frame 

" internal/primitive functions (probably has some duplicates)
if exists("r_package_internal") && r_package_internal != 0
  syn keyword rPrimitiveStatement stop warning gettext ngettext bindtextdomain .addCondHands
  syn keyword rPrimitiveStatement .resetCondHands .signalCondition .dfltStop .dfltWarn .addRestart
  syn keyword rPrimitiveStatement .getRestart .invokeRestart .addTryHandlers geterrmessage restart
  syn keyword rPrimitiveStatement function as.function.default .subset switch browser debug undebug
  syn keyword rPrimitiveStatement .primTrace .primUntrace .Internal on.exit Recall delay delayedAssign
  syn keyword rPrimitiveStatement .Primitive identical crossprod all any vector complex matrix length row
  syn keyword rPrimitiveStatement col c unlist cbind rbind drop oldClass class unclass names dimnames
  syn keyword rPrimitiveStatement all.names dim attributes attr comment get mget exists assign remove
  syn keyword rPrimitiveStatement duplicated unique which.min which.max match pmatch charmatch match.call
  syn keyword rPrimitiveStatement complete.cases attach detach search round atan log signif abs floor
  syn keyword rPrimitiveStatement ceiling sqrt sign trunc exp cos sin tan acos asin cosh sinh tanh acosh
  syn keyword rPrimitiveStatement asinh atanh lgamma gamma digamma trigamma pentagamma gammaCody lbeta
  syn keyword rPrimitiveStatement beta lchoose choose dchisq pchisq qchisq dexp pexp qexp dgeom pgeom
  syn keyword rPrimitiveStatement qgeom dpois ppois qpois dt pt qt dsignrank psignrank qsignrank besselJ
  syn keyword rPrimitiveStatement besselY psigamma Re Im Mod Arg Conj dbeta pbeta qbeta dbinom pbinom
  syn keyword rPrimitiveStatement qbinom dcauchy pcauchy qcauchy df pf qf dgamma pgamma qgamma dlnorm
  syn keyword rPrimitiveStatement plnorm qlnorm dlogis plogis qlogis dnbinom pnbinom qnbinom dnorm pnorm
  syn keyword rPrimitiveStatement qnorm dunif punif qunif dweibull pweibull qweibull dnchisq pnchisq
  syn keyword rPrimitiveStatement qnchisq dnt pnt qnt dwilcox pwilcox qwilcox besselI besselK dhyper
  syn keyword rPrimitiveStatement phyper qhyper dnbeta pnbeta qnbeta dnf pnf qnf dtukey ptukey qtukey
  syn keyword rPrimitiveStatement rchisq rexp rgeom rpois rt rsignrank rbeta rbinom rcauchy rf rgamma
  syn keyword rPrimitiveStatement rlnorm rlogis rnbinom rnchisq rnorm runif rweibull rwilcox rhyper
  syn keyword rPrimitiveStatement rmultinom sample RNGkind set.seed sum min max prod range cov cor cumsum
  syn keyword rPrimitiveStatement cumprod cummax cummin as.character as.vector paste format format.info
  syn keyword rPrimitiveStatement cat call do.call as.call type.convert as.environment nchar substr
  syn keyword rPrimitiveStatement substrgets strsplit abbreviate make.names grep sub gsub regexpr
  syn keyword rPrimitiveStatement grep.perl sub.perl gsub.perl regexpr.perl agrep tolower toupper chartr
  syn keyword rPrimitiveStatement sprintf make.unique charToRaw rawToChar rawShift intToBits rawToBits
  syn keyword rPrimitiveStatement packBits encodeString iconv strtrim is.null is.logical is.integer
  syn keyword rPrimitiveStatement is.real is.double is.complex is.character is.symbol is.environment
  syn keyword rPrimitiveStatement is.list is.pairlist is.expression is.object is.numeric is.matrix
  syn keyword rPrimitiveStatement is.array is.atomic is.recursive is.call is.language is.function
  syn keyword rPrimitiveStatement is.single is.vector is.na is.nan is.finite is.infinite proc.time gc.time
  syn keyword rPrimitiveStatement Version machine commandArgs int.unzip system system unlink help.start
  syn keyword rPrimitiveStatement show.help.item flush.console win.version shell.exec winDialog
  syn keyword rPrimitiveStatement winDialogString winMenuNames winMenuItems winMenuAdd winMenuDel
  syn keyword rPrimitiveStatement memory.size DLL.version bringToTop select.list readClipboard
  syn keyword rPrimitiveStatement writeClipboard chooseFiles getIdentification getWindowHandle
  syn keyword rPrimitiveStatement getWindowTitle setWindowTitle wsbrowser pkgbrowser data.manager
  syn keyword rPrimitiveStatement package.manager flush.console hsbrowser select.list aqua.custom.print
  syn keyword rPrimitiveStatement parse save saveToConn load loadFromConn serializeToConn
  syn keyword rPrimitiveStatement unserializeFromConn deparse dput dump substitute quote quit interactive
  syn keyword rPrimitiveStatement readline menu print.default prmatrix invisible gc gcinfo gctorture
  syn keyword rPrimitiveStatement memory.profile rep list split symbol.C symbol.For is.loaded .C .Fortran
  syn keyword rPrimitiveStatement .External .Call .External.graphics .Call.graphics recordGraphics
  syn keyword rPrimitiveStatement dyn.load dyn.unload ls typeof eval eval.with.vis expression sys.parent
  syn keyword rPrimitiveStatement sys.call sys.frame sys.nframe sys.calls sys.frames sys.on.exit
  syn keyword rPrimitiveStatement sys.parents sys.function parent.frame sort is.unsorted psort qsort
  syn keyword rPrimitiveStatement radixsort order rank missing nargs scan count.fields readTableHead
  syn keyword rPrimitiveStatement t.default aperm builtins edit dataentry args formals body bodyCode
  syn keyword rPrimitiveStatement globalenv environment reg.finalizer options sink sink.number lib.fixup
  syn keyword rPrimitiveStatement pos.to.env eapply lapply apply colSums colMeans rowSums rowMeans Rprof
  syn keyword rPrimitiveStatement object.size mem.limits merge capabilities new.env parent.env visibleflag
  syn keyword rPrimitiveStatement file.show file.edit file.create file.remove file.rename file.append
  syn keyword rPrimitiveStatement codeFiles.append file.symlink list.files file.exists file.choose
  syn keyword rPrimitiveStatement file.info file.access dir.create tempfile tempdir R.home date
  syn keyword rPrimitiveStatement index.search getenv putenv getwd setwd basename dirname Sys.info
  syn keyword rPrimitiveStatement Sys.sleep getlocale setlocale localeconv path.expand getpid
  syn keyword rPrimitiveStatement normalizePath fft mvfft nextn polyroot dev.control dev.copy dev.cur
  syn keyword rPrimitiveStatement device dev.next dev.off dev.prev dev.set rgb hsv hcl gray colors palette
  syn keyword rPrimitiveStatement plot.new plot.window axis plot.xy text mtext title abline box rect
  syn keyword rPrimitiveStatement polygon par readonly.pars segments arrows layout locator identify
  syn keyword rPrimitiveStatement strheight strwidth contour contourLines image dend dend.window replay
  syn keyword rPrimitiveStatement erase persp filledcontour getGPar playDL setGPar getSnapshot
  syn keyword rPrimitiveStatement playSnapshot symbols getGraphicsEvent inherits UseMethod NextMethod
  syn keyword rPrimitiveStatement standardGeneric nlm fmin zeroin optim optimhess terms.formula
  syn keyword rPrimitiveStatement update.formula model.frame model.matrix D deriv.default loadhistory
  syn keyword rPrimitiveStatement savehistory Sys.time as.POSIXct as.POSIXlt format.POSIXlt strptime
  syn keyword rPrimitiveStatement mkCode bcClose is.builtin.internal disassemble bcVersion load.from.file
  syn keyword rPrimitiveStatement save.to.file putconst stdin stdout stderr readLines writeLines readBin
  syn keyword rPrimitiveStatement writeBin readChar writeChar open isOpen isIncomplete isSeekable close
  syn keyword rPrimitiveStatement flush file url pipe fifo gzfile unz bzfile seek truncate pushBack
  syn keyword rPrimitiveStatement clearPushBackLength pushBackLength textConnection socketConnection
  syn keyword rPrimitiveStatement sockSelect getAllConnections summary.connection download nsl gzcon
  syn keyword rPrimitiveStatement readDCF getNumRtoCConverters getRtoCConverterDescriptions
  syn keyword rPrimitiveStatement getRtoCConverterStatus setToCConverterActiveStatus
  syn keyword rPrimitiveStatement removeToCConverterActiveStatus lockEnvironment environmentIsLocked
  syn keyword rPrimitiveStatement lockBinding unlockBinding bindingIsLocked makeActiveBinding
  syn keyword rPrimitiveStatement bindingIsActive mkUnbound isNamespaceEnv registerNamespace
  syn keyword rPrimitiveStatement unregisterNamespace getRegisteredNamespace getNamespaceRegistry
  syn keyword rPrimitiveStatement importIntoEnv write.table
endif

" base functions
if exists("r_package_base") && r_package_base != 0
  syn keyword rBaseStatement gammaCody besselI besselK besselJ besselY .Defunct Version provide getenv
  syn keyword rBaseStatement read.table.url scan.url source.url httpclient parse.dcf .Alias reshapeWide
  syn keyword rBaseStatement reshapeLong piechart machine Machine Platform restart printNoClass print.coefmat
  syn keyword rBaseStatement codes codes.factor codes.ordered anovalist.lm lm.fit.null lm.wfit.null glm.fit.null
  syn keyword rBaseStatement print.atomic La.eigen tetragamma pentagamma package.description .Deprecated loadURL
  syn keyword rBaseStatement delay La.svd La.chol geterrmessage try comment round signif logb beta lbeta gamma
  syn keyword rBaseStatement lgamma digamma trigamma psigamma factorial lfactorial choose lchoose R.Version
  syn keyword rBaseStatement commandArgs args cbind rbind .deparseOpts deparse do.call drop format.info gc gcinfo
  syn keyword rBaseStatement gctorture is.unsorted mem.limits nchar polyroot readline search searchpaths sprintf
  syn keyword rBaseStatement t.default typeof memory.profile capabilities inherits NextMethod data.class
  syn keyword rBaseStatement is.numeric.factor is.integer.factor encodeString RNGkind set.seed RNGversion .Script
  syn keyword rBaseStatement all.equal all.equal.default all.equal.numeric all.equal.character all.equal.factor
  syn keyword rBaseStatement all.equal.formula all.equal.language all.equal.list attr.all.equal all.names all.vars
  syn keyword rBaseStatement aperm append apply array slice.index as.logical as.logical.default as.integer
  syn keyword rBaseStatement as.integer.default as.double as.double.default as.complex as.complex.default
  syn keyword rBaseStatement as.single as.single.default as.character.default as.expression as.expression.default
  syn keyword rBaseStatement as.list as.list.default as.list.data.frame as.list.environment as.vector as.matrix
  syn keyword rBaseStatement as.matrix.default as.null as.null.default as.function as.function.default as.array
  syn keyword rBaseStatement as.symbol as.numeric assign attach detach ls autoload autoloader bquote forwardsolve
  syn keyword rBaseStatement backsolve lockEnvironment environmentIsLocked lockBinding bindingIsLocked
  syn keyword rBaseStatement makeActiveBinding bindingIsActive unlockBinding builtins by by.default by.data.frame
  syn keyword rBaseStatement print.by cat strsplit substr substring abbreviate make.names make.unique chartr
  syn keyword rBaseStatement tolower toupper casefold sQuote dQuote chol colSums colMeans rowSums rowMeans
  syn keyword rBaseStatement tryCatch withCallingHandlers suppressWarnings simpleCondition simpleError
  syn keyword rBaseStatement simpleWarning conditionMessage conditionCall conditionMessage.condition
  syn keyword rBaseStatement conditionCall.condition print.condition as.character.condition as.character.error
  syn keyword rBaseStatement signalCondition restartDescription restartFormals print.restart isRestart findRestart
  syn keyword rBaseStatement computeRestarts invokeRestart invokeRestartInteractively withRestarts
  syn keyword rBaseStatement .signalSimpleWarning .handleSimpleError conflicts stdin stdout stderr readLines
  syn keyword rBaseStatement writeLines open open.connection isOpen isIncomplete isSeekable close close.connection
  syn keyword rBaseStatement flush flush.connection file pipe fifo url gzfile unz bzfile socketConnection
  syn keyword rBaseStatement textConnection seek seek.connection truncate truncate.connection pushBack
  syn keyword rBaseStatement pushBackLength print.connection summary.connection showConnections getAllConnections
  syn keyword rBaseStatement getConnection closeAllConnections readBin writeBin readChar writeChar gzcon
  syn keyword rBaseStatement socketSelect contributors getNumCConverters getCConverterDescriptions
  syn keyword rBaseStatement getCConverterStatus setCConverterStatus removeCConverter cut cut.default data.matrix
  syn keyword rBaseStatement row.names row.names.data.frame row.names.default is.na.data.frame is.data.frame I
  syn keyword rBaseStatement print.AsIs t.data.frame dim.data.frame dimnames.data.frame as.data.frame
  syn keyword rBaseStatement as.data.frame.default as.data.frame.data.frame as.data.frame.list
  syn keyword rBaseStatement as.data.frame.vector as.data.frame.ts as.data.frame.character as.data.frame.matrix
  syn keyword rBaseStatement as.data.frame.model.matrix as.data.frame.array as.data.frame.AsIs data.frame
  syn keyword rBaseStatement xpdrows.data.frame cbind.data.frame rbind.data.frame print.data.frame
  syn keyword rBaseStatement as.matrix.data.frame Math.data.frame Ops.data.frame Summary.data.frame Sys.Date
  syn keyword rBaseStatement as.Date as.Date.POSIXct as.Date.POSIXlt as.Date.factor as.Date.character
  syn keyword rBaseStatement as.Date.default as.Date.date as.Date.dates format.Date print.Date summary.Date
  syn keyword rBaseStatement Ops.Date Math.Date Summary.Date as.character.Date c.Date mean.Date seq.Date cut.Date
  syn keyword rBaseStatement julian.Date weekdays.Date months.Date quarters.Date round.Date trunc.Date rep.Date
  syn keyword rBaseStatement diff.Date Sys.time Sys.timezone as.POSIXlt as.POSIXct as.POSIXct.Date as.POSIXct.date
  syn keyword rBaseStatement as.POSIXct.dates as.POSIXct.POSIXlt as.POSIXct.default format.POSIXlt strptime
  syn keyword rBaseStatement format.POSIXct print.POSIXct print.POSIXlt summary.POSIXct summary.POSIXlt Ops.POSIXt
  syn keyword rBaseStatement Math.POSIXt Summary.POSIXct Summary.POSIXlt as.character.POSIXt str.POSIXt
  syn keyword rBaseStatement is.na.POSIXlt c.POSIXct c.POSIXlt all.equal.POSIXct ISOdatetime ISOdate
  syn keyword rBaseStatement as.matrix.POSIXlt mean.POSIXct mean.POSIXlt difftime as.difftime print.difftime
  syn keyword rBaseStatement round.difftime Ops.difftime Math.difftime mean.difftime Summary.difftime seq.POSIXt
  syn keyword rBaseStatement cut.POSIXt julian julian.POSIXt weekdays weekdays.POSIXt months months.POSIXt
  syn keyword rBaseStatement quarters quarters.POSIXt trunc.POSIXt round.POSIXt as.data.frame.POSIXlt rep.POSIXct
  syn keyword rBaseStatement rep.POSIXlt diff.POSIXt read.dcf write.dcf delayedAssign diag diff diff.default dput
  syn keyword rBaseStatement dget dump duplicated duplicated.default duplicated.data.frame duplicated.matrix
  syn keyword rBaseStatement unique unique.default unique.data.frame unique.matrix dyn.load dyn.unload
  syn keyword rBaseStatement getNativeSymbolInfo getLoadedDLLs getDLLRegisteredRoutines
  syn keyword rBaseStatement getDLLRegisteredRoutines.character getDLLRegisteredRoutines.DLLInfo
  syn keyword rBaseStatement print.NativeRoutineList print.DLLRegisteredRoutines getCallingDLL print.DLLInfo
  syn keyword rBaseStatement print.DLLInfoList eapply eigen environment .GlobalEnv eval eval.parent evalq new.env
  syn keyword rBaseStatement parent.env local Recall with with.default force exists expand.grid factor is.factor
  syn keyword rBaseStatement as.factor category levels nlevels as.vector.factor as.character.factor print.factor
  syn keyword rBaseStatement Math.factor Summary.factor Ops.factor ordered is.ordered as.ordered Ops.ordered
  syn keyword rBaseStatement R.home file.show file.append file.remove file.rename list.files file.path file.exists
  syn keyword rBaseStatement file.create file.choose file.copy file.symlink file.info file.access dir.create
  syn keyword rBaseStatement format.octmode print.octmode system.file getwd setwd basename dirname Sys.info
  syn keyword rBaseStatement Sys.sleep path.expand findInterval formals body alist format format.default
  syn keyword rBaseStatement format.char format.pval formatC format.factor format.data.frame format.AsIs prettyNum
  syn keyword rBaseStatement subset.data.frame subset subset.default subset.matrix transform.data.frame transform
  syn keyword rBaseStatement transform.default stack.data.frame stack stack.default unstack.data.frame unstack
  syn keyword rBaseStatement unstack.default get mget Sys.getenv Sys.putenv Sys.getpid gl grep sub gsub regexpr
  syn keyword rBaseStatement agrep identical isTRUE ifelse interaction is.vector is.primitive jitter kappa
  syn keyword rBaseStatement kappa.lm kappa.default kappa.qr kappa.tri kronecker labels labels.default lapply
  syn keyword rBaseStatement lapply lazyLoad testPlatformEquivalence library print.libraryIQR library.dynam
  syn keyword rBaseStatement library.dynam.unload require .packages .path.package .find.package print.packageInfo
  syn keyword rBaseStatement manglePackageName .getRequiredPackages licence load save save.image sys.load.image
  syn keyword rBaseStatement sys.save.image Sys.getlocale Sys.setlocale Sys.localeconv lower.tri mapply match
  syn keyword rBaseStatement match match.call pmatch match.arg charmatch char.expand match.fun matrix nrow ncol
  syn keyword rBaseStatement NROW NCOL rownames colnames row col crossprod t t.data.frame max.col mean
  syn keyword rBaseStatement mean.default mean.data.frame merge merge.default merge.data.frame simpleMessage
  syn keyword rBaseStatement suppressMessages message trace untrace .isMethodsDispatchOn tracingState mode
  syn keyword rBaseStatement storage.mode names names.default getNamespace loadedNamespaces getNamespaceName
  syn keyword rBaseStatement getNamespaceVersion getNamespaceExports getNamespaceImports getNamespaceUsers
  syn keyword rBaseStatement getExportedValue attachNamespace loadNamespace loadingNamespaceInfo
  syn keyword rBaseStatement saveNamespaceImage topenv unloadNamespace .Import .ImportFrom .Export isNamespace
  syn keyword rBaseStatement isBaseNamespace getNamespaceInfo setNamespaceInfo asNamespace namespaceImport
  syn keyword rBaseStatement namespaceImportFrom namespaceImportClasses namespaceImportMethods importIntoEnv
  syn keyword rBaseStatement namespaceExport .mergeExportMethods packageHasNamespace parseNamespaceFile
  syn keyword rBaseStatement .NotYetImplemented .NotYetUsed options getOption outer getRversion as.pairlist
  syn keyword rBaseStatement pairlist parse paste pmax pmin pretty print print.default prmatrix noquote
  syn keyword rBaseStatement as.matrix.noquote c.noquote print.noquote print.listof print.simple.list is.qr qr
  syn keyword rBaseStatement qr.coef qr.qy qr.qty qr.resid qr.fitted qr.Q qr.R qr.X quit q range.default rank raw
  syn keyword rBaseStatement as.raw charToRaw rawToChar rawShift rawToBits intToBits packBits count.fields
  syn keyword rBaseStatement type.convert read.table read.csv read.delim rep rep.default rep.int replace replicate
  syn keyword rBaseStatement rev rev.default rle print.rle inverse.rle rm remove rowsum.default rowsum.data.frame
  syn keyword rBaseStatement sample sapply scale scale.default scan seq seq.default sequence .saveRDS .readRDS
  syn keyword rBaseStatement serialize unserialize union intersect setdiff setequal is.element sink sink.number
  syn keyword rBaseStatement solve.qr solve.default solve qr.solve sort order sort.list source sys.source split
  syn keyword rBaseStatement split.default split.data.frame unsplit stop stopifnot warning gettext bindtextdomain
  syn keyword rBaseStatement ngettext gettextf strtrim strwrap formatDL sum min max prod all any summary
  syn keyword rBaseStatement summary.default summary.factor summary.matrix summary.data.frame svd sweep switch
  syn keyword rBaseStatement sys.call sys.calls sys.frame sys.function sys.frames sys.nframe sys.parent
  syn keyword rBaseStatement sys.parents sys.status sys.on.exit table print.table summary.table
  syn keyword rBaseStatement print.summary.table as.data.frame.table is.table as.table as.table.default prop.table
  syn keyword rBaseStatement margin.table tabulate tapply addTaskCallback removeTaskCallback getTaskCallbackNames
  syn keyword rBaseStatement taskCallbackManager tempfile tempdir system.time date toString toString.default
  syn keyword rBaseStatement traceback system unix unlink zip.file.extract unlist unname upper.tri packageEvent
  syn keyword rBaseStatement getHook setHook mat.or.vec is.R shQuote vector logical character integer double
  syn keyword rBaseStatement complex single warnings which which.min which.max write write.table write.csv xor
  syn keyword rBaseStatement zapsmall is.na c
endif

" graphics functions
if exists("r_package_graphics") && r_package_graphics != 0
  syn keyword rGraphicsStatement abline arrows assocplot axis axTicks barplot barplot.default box boxplot
  syn keyword rGraphicsStatement boxplot.default boxplot.formula boxplot.stats bxp chull contourLines contour
  syn keyword rGraphicsStatement contour.default co.intervals panel.smooth coplot curve axis.POSIXct plot.POSIXct
  syn keyword rGraphicsStatement plot.POSIXlt hist.POSIXt axis.Date plot.Date hist.Date dotchart filled.contour
  syn keyword rGraphicsStatement fourfoldplot grid hist hist.default plot.histogram lines.histogram nclass.Sturges
  syn keyword rGraphicsStatement nclass.scott nclass.FD identify identify.default image image.default lcm layout
  syn keyword rGraphicsStatement layout.show legend lines lines.default locator matpoints matlines matplot mosaicplot
  syn keyword rGraphicsStatement mosaicplot.default mosaicplot.formula mtext pairs pairs.formula pairs.default par
  syn keyword rGraphicsStatement persp persp.default pie xy.coords plot plot.function plot.default plot.factor
  syn keyword rGraphicsStatement plot.table plot.formula lines.formula points.formula plot.xy plot.new plot.window
  syn keyword rGraphicsStatement plot.data.frame .newplot.hook plot.design points points.default polygon rect rug
  syn keyword rGraphicsStatement .SSget .SSexists .SSassign split.screen screen erase.screen close.screen segments
  syn keyword rGraphicsStatement stars stem stripchart strwidth strheight sunflowerplot symbols text text.default
  syn keyword rGraphicsStatement title xinch yinch xyinch xyz.coords
endif

" stats functions
if exists("r_package_stats") && r_package_stats != 0
  syn keyword rStatsStatement AIC AIC.logLik AIC.default ARMAacf ARMAtoMA C HoltWinters predict.HoltWinters
  syn keyword rStatsStatement residuals.HoltWinters plot.HoltWinters print.HoltWinters decompose plot.decomposed.ts
  syn keyword rStatsStatement KalmanLike KalmanRun KalmanForecast KalmanSmooth StructTS print.StructTS
  syn keyword rStatsStatement predict.StructTS tsdiag.StructTS tsSmooth tsSmooth.StructTS TukeyHSD TukeyHSD.aov
  syn keyword rStatsStatement print.TukeyHSD plot.TukeyHSD acf pacf pacf.default plot.acf ccf print.acf add.scope
  syn keyword rStatsStatement drop.scope factor.scope step extractAIC extractAIC.coxph extractAIC.survreg
  syn keyword rStatsStatement extractAIC.glm extractAIC.lm extractAIC.negbin addmargins aggregate aggregate.default
  syn keyword rStatsStatement aggregate.data.frame aggregate.ts printCoefmat print.anova ansari.test
  syn keyword rStatsStatement ansari.test.default ansari.test.formula aov print.aov summary.aov print.summary.aov
  syn keyword rStatsStatement coef.aov alias alias.formula alias.lm print.aovlist summary.aovlist
  syn keyword rStatsStatement print.summary.aovlist coef.listof se.contrast se.contrast.aov se.contrast.aovlist
  syn keyword rStatsStatement approx approxfun findInterval ar ar.yw ar.yw.default print.ar predict.ar ar.burg
  syn keyword rStatsStatement ar.burg.default ar.mle ar.ols arima print.Arima predict.Arima makeARIMA coef.Arima
  syn keyword rStatsStatement vcov.Arima logLik.Arima tsdiag.Arima tsdiag ave bw.nrd bw.SJ bw.ucv bw.bcv
  syn keyword rStatsStatement bartlett.test bartlett.test.default bartlett.test.formula binom.test biplot
  syn keyword rStatsStatement biplot.default biplot.princomp biplot.prcomp qbirthday pbirthday cancor chisq.test
  syn keyword rStatsStatement cmdscale complete.cases confint confint.lm confint.glm confint.nls confint.default
  syn keyword rStatsStatement constrOptim contr.poly poly predict.poly makepredictcall.poly polym contrasts
  syn keyword rStatsStatement contr.helmert contr.treatment contr.sum contr.SAS cor cov var cor.test
  syn keyword rStatsStatement cor.test.default cor.test.formula cov.wt cpgram cutree as.dendrogram
  syn keyword rStatsStatement as.dendrogram.hclust .memberDend .midDend midcache.dendrogram print.dendrogram
  syn keyword rStatsStatement str.dendrogram plot.dendrogram plotNode plotNodeLimit cut.dendrogram is.leaf
  syn keyword rStatsStatement order.dendrogram reorder reorder.dendrogram rev.dendrogram labels.dendrogram
  syn keyword rStatsStatement dendrapply heatmap density plot.density print.density D deriv deriv.formula
  syn keyword rStatsStatement deriv.default diffinv diffinv.vector diffinv.default diffinv.ts toeplitz dist
  syn keyword rStatsStatement format.dist as.matrix.dist as.dist as.dist.default print.dist dexp pexp qexp rexp
  syn keyword rStatsStatement dunif punif qunif runif dnorm pnorm qnorm rnorm dcauchy pcauchy qcauchy rcauchy
  syn keyword rStatsStatement dgamma pgamma qgamma rgamma dlnorm plnorm qlnorm rlnorm dlogis plogis qlogis rlogis
  syn keyword rStatsStatement dweibull pweibull qweibull rweibull dbeta pbeta qbeta rbeta dbinom pbinom qbinom
  syn keyword rStatsStatement rbinom dmultinom rmultinom dchisq pchisq qchisq rchisq df pf qf rf dgeom pgeom qgeom
  syn keyword rStatsStatement rgeom dhyper phyper qhyper rhyper dnbinom pnbinom qnbinom rnbinom dpois ppois qpois
  syn keyword rStatsStatement rpois dt pt qt rt ptukey qtukey dwilcox pwilcox qwilcox rwilcox dsignrank psignrank
  syn keyword rStatsStatement qsignrank rsignrank dummy.coef dummy.coef.lm dummy.coef.aovlist ecdf print.ecdf
  syn keyword rStatsStatement summary.ecdf plot.ecdf embed expand.model.frame factanal factanal.fit.mle
  syn keyword rStatsStatement print.loadings print.factanal varimax promax family print.family power make.link
  syn keyword rStatsStatement poisson quasipoisson gaussian binomial quasibinomial Gamma inverse.gaussian quasi fft
  syn keyword rStatsStatement mvfft nextn convolve filter fisher.test fivenum fligner.test fligner.test.default
  syn keyword rStatsStatement fligner.test.formula friedman.test friedman.test.default friedman.test.formula ftable
  syn keyword rStatsStatement ftable.default ftable.formula as.table.ftable write.ftable print.ftable read.ftable
  syn keyword rStatsStatement glm glm.control glm.fit print.glm anova.glm anova.glmlist stat.anova summary.glm
  syn keyword rStatsStatement print.summary.glm deviance.glm effects.glm family.glm residuals.glm model.frame.glm
  syn keyword rStatsStatement weights.glm formula.glm hclust plot.hclust plclust as.hclust as.hclust.default
  syn keyword rStatsStatement as.hclust.twins print.hclust cophenetic cophenetic.default cophenetic.dendrogram
  syn keyword rStatsStatement print.htest rect.hclust identify.hclust integrate print.integrate interaction.plot
  syn keyword rStatsStatement isoreg fitted.isoreg residuals.isoreg print.isoreg plot.isoreg kernel print.tskernel
  syn keyword rStatsStatement plot.tskernel df.kernel bandwidth.kernel is.tskernel kernapply kernapply.vector
  syn keyword rStatsStatement kernapply.default kernapply.ts kernapply.tskernel kmeans print.kmeans kruskal.test
  syn keyword rStatsStatement kruskal.test.default kruskal.test.formula ks.test ksmooth lag lag.default lag.plot lm
  syn keyword rStatsStatement lm.fit lm.wfit print.lm summary.lm print.summary.lm residuals.lm weights.default
  syn keyword rStatsStatement deviance.lm formula.lm family.lm model.frame.lm variable.names.lm case.names.lm
  syn keyword rStatsStatement anova.lm anova.lmlist predict.lm effects.lm model.matrix.lm predict.mlm labels.lm hat
  syn keyword rStatsStatement weighted.residuals lm.influence influence influence.lm influence.glm hatvalues
  syn keyword rStatsStatement hatvalues.lm rstandard rstandard.lm rstandard.glm rstudent rstudent.lm rstudent.glm
  syn keyword rStatsStatement dffits dfbeta dfbeta.lm dfbetas dfbetas.lm covratio cooks.distance cooks.distance.lm
  syn keyword rStatsStatement cooks.distance.glm influence.measures print.infl summary.infl loess loess.control
  syn keyword rStatsStatement simpleLoess predict.loess predLoess pointwise print.loess summary.loess
  syn keyword rStatsStatement print.summary.loess scatter.smooth loess.smooth anova.loess logLik print.logLik
  syn keyword rStatsStatement str.logLik as.data.frame.logLik logLik.glm logLik.lm loglin lowess lsfit ls.diag
  syn keyword rStatsStatement ls.print mad mahalanobis manova summary.manova print.summary.manova mantelhaen.test
  syn keyword rStatsStatement mcnemar.test median medpolish print.medpolish plot.medpolish summary.mlm SSD estVar
  syn keyword rStatsStatement SSD.mlm estVar.SSD estVar.mlm Tr proj.matrix Rank Thin.row Thin.col mauchley.test
  syn keyword rStatsStatement mauchley.test.mlm mauchley.test.SSD sphericity anova.mlm Pillai Wilks HL Roy
  syn keyword rStatsStatement anova.mlmlist deviance.mlm plot.mlm model.tables model.tables.aov se.aov
  syn keyword rStatsStatement model.tables.aovlist se.aovlist make.tables.aovproj make.tables.aovprojlist
  syn keyword rStatsStatement replications eff.aovlist model.frame.aovlist print.mtable formula formula.default
  syn keyword rStatsStatement formula.formula formula.terms formula.data.frame print.formula as.formula terms
  syn keyword rStatsStatement terms.default terms.terms print.terms labels.terms delete.response reformulate
  syn keyword rStatsStatement drop.terms terms.formula coef coef.default residuals residuals.default deviance
  syn keyword rStatsStatement deviance.default fitted fitted.default anova effects weights df.residual
  syn keyword rStatsStatement df.residual.default variable.names variable.names.default case.names
  syn keyword rStatsStatement case.names.default offset .checkMFClasses .MFclass model.frame model.frame.default
  syn keyword rStatsStatement model.weights model.offset model.matrix model.matrix.default model.response
  syn keyword rStatsStatement model.extract preplot update is.empty.model makepredictcall makepredictcall.default
  syn keyword rStatsStatement .getXlevels monthplot monthplot.StructTS monthplot.stl monthplot.ts monthplot.default
  syn keyword rStatsStatement mood.test mood.test.default mood.test.formula na.contiguous na.contiguous.default
  syn keyword rStatsStatement na.pass na.action na.action.default na.fail na.fail.default na.omit na.omit.default
  syn keyword rStatsStatement na.omit.data.frame na.exclude na.exclude.default na.exclude.data.frame naresid
  syn keyword rStatsStatement naresid.default naresid.exclude naprint naprint.default naprint.exclude napredict
  syn keyword rStatsStatement napredict.default napredict.exclude nlm optimize uniroot profiler profiler.nls
  syn keyword rStatsStatement profile.nls plot.profile.nls numericDeriv nlsModel.plinear nlsModel nls.control nls
  syn keyword rStatsStatement coef.nls print.nls summary.nls print.summary.nls coef.summary.nls weights.nls
  syn keyword rStatsStatement predict.nls fitted.nls formula.nls residuals.nls logLik.nls df.residual.nls
  syn keyword rStatsStatement deviance.nls vcov.nls anova.nls anovalist.nls asOneSidedFormula setNames clearNames
  syn keyword rStatsStatement oneway.test optim p.adjust pairwise.t.test pairwise.wilcox.test pairwise.prop.test
  syn keyword rStatsStatement pairwise.table print.pairwise.htest plot.lm power.t.test power.prop.test
  syn keyword rStatsStatement print.power.htest power.anova.test ppoints ppr ppr.formula ppr.default print.ppr
  syn keyword rStatsStatement summary.ppr print.summary.ppr plot.ppr predict.ppr prcomp prcomp.default
  syn keyword rStatsStatement prcomp.formula plot.prcomp print.prcomp summary.prcomp print.summary.prcomp
  syn keyword rStatsStatement predict.prcomp predict predict.default predict.glm predict.princomp summary.princomp
  syn keyword rStatsStatement print.summary.princomp plot.princomp screeplot loadings princomp princomp.formula
  syn keyword rStatsStatement princomp.default print.princomp profile proj proj.default proj.lm proj.aov
  syn keyword rStatsStatement proj.aovlist terms.aovlist prop.test prop.trend.test qqnorm qqnorm.default qqline
  syn keyword rStatsStatement qqplot quade.test quade.test.default quade.test.formula quantile quantile.default IQR
  syn keyword rStatsStatement relevel relevel.default relevel.ordered relevel.factor reorder.factor reshape runmed
  syn keyword rStatsStatement smoothEnds sd selfStart selfStart.default selfStart.formula getInitial
  syn keyword rStatsStatement getInitial.formula getInitial.selfStart getInitial.default sortedXyData
  syn keyword rStatsStatement sortedXyData.default NLSstClosestX NLSstClosestX.sortedXyData NLSstRtAsymptote
  syn keyword rStatsStatement NLSstRtAsymptote.sortedXyData NLSstLfAsymptote NLSstLfAsymptote.sortedXyData
  syn keyword rStatsStatement NLSstAsymptotic NLSstAsymptotic.sortedXyData shapiro.test smooth print.tukeysmooth
  syn keyword rStatsStatement summary.tukeysmooth smooth.spline print.smooth.spline predict.smooth.spline
  syn keyword rStatsStatement predict.smooth.spline.fit supsmu spectrum spec.taper spec.ar spec.pgram plot.spec
  syn keyword rStatsStatement plot.spec.coherency plot.spec.phase spline splinefun stepfun is.stepfun as.stepfun
  syn keyword rStatsStatement as.stepfun.default knots knots.stepfun print.stepfun summary.stepfun plot.stepfun
  syn keyword rStatsStatement lines.stepfun as.stepfun.isoreg stl print.stl summary.stl plot.stl symnum t.test
  syn keyword rStatsStatement t.test.default t.test.formula termplot Box.test PP.test start end frequency time
  syn keyword rStatsStatement window cycle deltat ts tsp hasTsp is.ts as.ts as.ts.default .cbind.ts .makeNamesTs
  syn keyword rStatsStatement Ops.ts cbind.ts ts.union ts.intersect diff.ts na.omit.ts is.mts start.default
  syn keyword rStatsStatement end.default frequency.default deltat.default time.default time.ts cycle.default
  syn keyword rStatsStatement cycle.ts print.ts plot.ts lines.ts window.default window.ts t.ts ts.plot arima.sim
  syn keyword rStatsStatement line update.default update.formula var.test var.test.default var.test.formula vcov
  syn keyword rStatsStatement vcov.glm vcov.lm vcov.mlm vcov.gls vcov.lme weighted.mean wilcox.test
  syn keyword rStatsStatement wilcox.test.default wilcox.test.formula xtabs print.xtabs SSasymp SSgompertz
  syn keyword rStatsStatement SSweibull .onLoad .onUnload
endif

" tools functions
if exists("r_package_tools") && r_package_tools != 0
  syn keyword rToolsStatement undoc print.undoc codoc print.codoc codocClasses print.codocClasses codocData
  syn keyword rToolsStatement print.codocData checkDocFiles print.checkDocFiles checkDocStyle print.checkDocStyle
  syn keyword rToolsStatement checkFF print.checkFF checkReplaceFuns print.checkReplaceFuns checkTnF print.checkTnF
  syn keyword rToolsStatement as.alist.call as.alist.symbol Rdinfo Rdcontents Rdindex checkVignettes
  syn keyword rToolsStatement print.checkVignettes pkgVignettes buildVignettes vignetteMetaRE vignetteInfo
  syn keyword rToolsStatement .writeVignetteHtmlIndex vignetteDepends getVigDepMtrx makeLazyLoadDB makeLazyLoading
  syn keyword rToolsStatement package.dependencies pkgDepends getDepList isSatisfied buildDepList getDepMtrx
  syn keyword rToolsStatement getRemotePkgDepends installedDepends foundDepends compareDependsPkgVersion
  syn keyword rToolsStatement reduceDepends depMtrxToStrings installFoundDepends filePathAsAbsolute filePathSansExt
  syn keyword rToolsStatement fileTest listFilesWithExts listFilesWithType delimMatch .OStype xgettext
  syn keyword rToolsStatement print.xgettext print.xngettext xngettext .onUnload
endif

" methods functions
if exists("r_package_methods") && r_package_methods != 0
  syn keyword rMethodsStatement .InitBasicClassMethods .BasicFunsList .addBasicGeneric genericForPrimitive
  syn keyword rMethodsStatement setGenericForPrimitive .findBasicFuns .InitExtensions .simpleExtCoerce
  syn keyword rMethodsStatement .simpleIsCoerce .simpleExtTest .simpleExtReplace .InhSlotNames .dataPartReplace
  syn keyword rMethodsStatement .ErrorReplace .objectSlotNames makeExtends .findAll .InitClassUnion setClassUnion
  syn keyword rMethodsStatement isClassUnion setGeneric isGeneric removeGeneric getMethods getMethodsForDispatch
  syn keyword rMethodsStatement .setIfBase .getMethodsForDispatch .setMethodsForDispatch cacheMethod setMethod
  syn keyword rMethodsStatement removeMethod findMethod getMethod dumpMethod selectMethod hasMethod existsMethod
  syn keyword rMethodsStatement dumpMethods signature showMethods removeMethodsObject removeMethods resetGeneric
  syn keyword rMethodsStatement setReplaceMethod setGroupGeneric isGroup callGeneric initMethodDispatch
  syn keyword rMethodsStatement isSealedMethod .lockedForMethods MethodsList makeMethodsList SignatureMethod
  syn keyword rMethodsStatement insertMethod MethodsListSelect emptyMethodsList insertMethodInEmptyList
  syn keyword rMethodsStatement finalDefaultMethod inheritedSubMethodLists matchSignature showMlist promptMethods
  syn keyword rMethodsStatement linearizeMlist print.MethodsList listFromMlist .insertCachedMethods .addMethodFrom
  syn keyword rMethodsStatement asMethodDefinition .InitMethodsListClass .InitMethodDefinitions .MakeSignature
  syn keyword rMethodsStatement .findNextMethod .hasCallNextMethod callNextMethod loadMethod .doSubNextCall
  syn keyword rMethodsStatement testVirtual makePrototypeFromClassDef newEmptyObject completeClassDefinition
  syn keyword rMethodsStatement .completeClassSlots .uncompleteClassDefinition .isIndirectExtension .mergeSlots
  syn keyword rMethodsStatement getAllSuperClasses superClassDepth isVirtualClass assignClassDef .InitClassDefinition
  syn keyword rMethodsStatement .initClassSupport newBasic defaultPrototype reconcilePropertiesAndPrototype tryNew
  syn keyword rMethodsStatement empty.dump isClassDef showClass showExtends print.classRepresentation possibleExtends
  syn keyword rMethodsStatement .possibleExtends completeExtends completeSubclasses .walkClassGraph classMetaName
  syn keyword rMethodsStatement methodsPackageMetaName requireMethods .missingMethod getSlots validSlotNames
  syn keyword rMethodsStatement getDataPart setDataPart .validDataPartClass .mergeAttrs .newExternalptr
  syn keyword rMethodsStatement .transitiveExtends .transitiveSubclasses .combineExtends .simpleCoerceExpr
  syn keyword rMethodsStatement .simpleReplaceExpr newClassRepresentation .tempClassDef .newClassRepresentation
  syn keyword rMethodsStatement .insertExpr substituteFunctionArgs .makeValidityMethod .mergeClassDefSlots
  syn keyword rMethodsStatement ..mergeClassDefSlots .gblEnv ..isPrototype .isPrototype .className .requirePackage
  syn keyword rMethodsStatement ..requirePackage .classDefEnv .asEnvironmentPackage .classEnv ..classEnv .genEnv
  syn keyword rMethodsStatement .makeGeneric makeGeneric makeStandardGeneric generic.skeleton defaultDumpName
  syn keyword rMethodsStatement getAllMethods mergeMethods doPrimitiveMethod conformMethod rematchDefinition
  syn keyword rMethodsStatement unRematchDefinition getGeneric .getGeneric getGroup getMethodsMetaData
  syn keyword rMethodsStatement assignMethodsMetaData mlistMetaName getGenerics .getGenerics cacheMetaData
  syn keyword rMethodsStatement cacheGenericsMetaData setPrimitiveMethods findUnique MethodAddCoerce missingArg
  syn keyword rMethodsStatement balanceMethodsList sigToEnv .methodSignatureMatrix .valueClassTest
  syn keyword rMethodsStatement .getOrMakeMethodsList .makeCallString .ValidateValueClass .asGroupArgument
  syn keyword rMethodsStatement metaNameUndo .recursiveCallTest .NonstandardGenericTest .GenericInPrimitiveMethods
  syn keyword rMethodsStatement .signatureString .ChangeFormals .envSearch .genericName .genericEnv
  syn keyword rMethodsStatement .externalCallerEnv .parentEnvList .genericAssign .derivedDefaultMethod .identC
  syn keyword rMethodsStatement .matchBasic matchDefaults getGroupMembers deletePrimMethods setClass representation
  syn keyword rMethodsStatement prototype .prototype makeClassRepresentation getClassDef getClass slot
  syn keyword rMethodsStatement checkSlotAssignment slotNames .slotNames removeClass isClass new getClasses
  syn keyword rMethodsStatement validObject setValidity resetClass initialize findClass isSealedClass sealClass
  syn keyword rMethodsStatement sessionData traceOn traceOff browseAll functionBody .ff allNames getFunction el
  syn keyword rMethodsStatement elNamed formalArgs findFunction existsFunction Quote .message hasArg as
  syn keyword rMethodsStatement .quickCoerceSelect .asFromReplace setAs .setCoerceGeneric .basicCoerceMethod
  syn keyword rMethodsStatement .makeAsMethod .removePreviousCoerce is extends setIs .validExtends languageEl
  syn keyword rMethodsStatement isGrammarSymbol .makeBasicFuns .InitSubsetMethods setOldClass .oldTestFun
  syn keyword rMethodsStatement .oldCoerceFun .oldReplaceFun .setOldIs getPackageName setPackageName packageSlot
  syn keyword rMethodsStatement .makeCallString showDefault show .InitShowMethods classLabel substituteDirect
  syn keyword rMethodsStatement .TraceWithMethods .makeTracedFunction .untracedFunction .InitTraceFunctions
  syn keyword rMethodsStatement .doTracePrint .traceClassName trySilent .assignOverBinding .setMethodOverBinding
  syn keyword rMethodsStatement .searchNamespaceNames .findFunEnvAndName ..First.lib .onLoad .onUnload .onAttach
  syn keyword rMethodsStatement .Last.lib .Last.lib
endif

" utils functions
if exists("r_package_utils") && r_package_utils != 0
  syn keyword rUtilsStatement RSiteSearch Rprof Sweave SweaveReadFile SweaveGetSyntax SweaveSyntConv
  syn keyword rUtilsStatement SweaveParseOptions SweaveHooks RweaveLatex RweaveLatexSetup RweaveLatexRuncode
  syn keyword rUtilsStatement RweaveLatexWritedoc RweaveLatexFinish RweaveLatexOptions RweaveChunkPrefix
  syn keyword rUtilsStatement RweaveEvalWithOpt RweaveTryStop Stangle Rtangle RtangleSetup RtangleRuncode
  syn keyword rUtilsStatement RtangleWritedoc RtangleFinish alarm apropos find citEntry citHeader citFooter
  syn keyword rUtilsStatement readCitationFile print.citation print.citationList person as.person as.person.default
  syn keyword rUtilsStatement personList as.personList as.personList.person as.personList.default
  syn keyword rUtilsStatement as.character.person as.character.personList toBibtex.person toBibtex.personList
  syn keyword rUtilsStatement toBibtex.citation toBibtex.citationList citation data browseEnv wsbrowser de.ncols
  syn keyword rUtilsStatement de.setup de.restore de data.entry dump.frames debugger limitedLabels recover demo
  syn keyword rUtilsStatement dataentry edit edit.default edit.data.frame edit.matrix file.edit vi emacs xemacs
  syn keyword rUtilsStatement xedit pico example head head.default head.data.frame head.function tail tail.default
  syn keyword rUtilsStatement tail.data.frame tail.matrix tail.function help help.search print.hsearch
  syn keyword rUtilsStatement printhsearchInternal loadhistory savehistory history iconv iconvlist localeToCharset
  syn keyword rUtilsStatement packageDescription print.packageDescription index.search print.packageIQR menu
  syn keyword rUtilsStatement getCRANmirrors checkCRAN object.size findGeneric methods print.MethodsFunction
  syn keyword rUtilsStatement getFromNamespace assignInNamespace fixInNamespace getAnywhere print.getAnywhere
  syn keyword rUtilsStatement package.skeleton packageStatus summary.packageStatus print.packageStatus
  syn keyword rUtilsStatement update.packageStatus upgrade upgrade.packageStatus available.packages CRAN.packages
  syn keyword rUtilsStatement simplifyRepos update.packages old.packages new.packages installed.packages
  syn keyword rUtilsStatement remove.packages download.packages contrib.url chooseCRANmirror setRepositories
  syn keyword rUtilsStatement normalizePath compareVersion install.packages page prompt prompt.default
  syn keyword rUtilsStatement prompt.data.frame promptData topicName .helpForCall .tryHelp read.fortran read.fwf
  syn keyword rUtilsStatement url.show sessionInfo print.sessionInfo toLatex.sessionInfo print.socket make.socket
  syn keyword rUtilsStatement close.socket read.socket write.socket str str.data.frame str.default ls.str lsf.str
  syn keyword rUtilsStatement summaryRprof toBibtex print.Bibtex toLatex print.Latex bug.report download.file nsl
  syn keyword rUtilsStatement help.start browseURL make.packages.html link.html.help package.contents vignette
  syn keyword rUtilsStatement print.vignette edit.vignette select.list flush.console
endif

" Special
syn match rDelimiter /[,;:]/

" Error
syn region rParenRegion matchgroup=rParenDelimiter start=/(/ end=/)/ transparent contains=ALLBUT,rError,rBraceError,rCurlyError
syn region rCurlyRegion matchgroup=rCurlyDelimiter start=/{/ end=/}/ transparent contains=ALLBUT,rError,rBraceError,rParenError fold
syn region rBraceRegion matchgroup=rBraceDelimiter start=/\[/ end=/]/ transparent contains=ALLBUT,rError,rCurlyError,rParenError
syn match rError      /[)\]}]/
syn match rBraceError /[)}]/ contained
syn match rCurlyError /[)\]]/ contained
syn match rParenError /[\]}]/ contained

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_r_syn_inits")
  if version < 508
    let did_r_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink rComment                 Comment      
  HiLink rConstant                Constant
  HiLink rString                  String
  HiLink rNumber                  Number
  HiLink rBoolean                 Boolean
  HiLink rFloat                   Float
  HiLink rStatement               Statement
  HiLink rBaseStatement           Statement                 
  HiLink rStatsStatement          Statement                 
  HiLink rGraphicsStatement       Statement                               
  HiLink rToolsStatement          Statement                               
  HiLink rMethodsStatement        Statement                               
  HiLink rUtilsStatement          Statement                               
  HiLink rPrimitiveStatement      Statement                               
  HiLink rConditional             Conditional
  HiLink rRepeat                  Repeat
  HiLink rIdentifier              Identifier
  HiLink rFunction                Function
  HiLink rArrow                   Statement
  HiLink rType	                  Type
  HiLink rDelimiter               Delimiter
  HiLink rStringDelimiter         Delimiter
  HiLink rParenDelimiter          Delimiter
  HiLink rCurlyDelimiter          Delimiter
  HiLink rBraceDelimiter          Delimiter
  HiLink rError                   Error
  HiLink rParenError              Error
  HiLink rCurlyError              Error
  HiLink rBraceError              Error
  delcommand HiLink
endif

let b:current_syntax="r"
" vim: ts=8 sw=2
