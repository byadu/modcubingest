addstore<- function(est_storeid, est_storename, est_storetype, est_dirdsn, est_dbdirid) {
	dbWriteTable(configdb, "etl_store", connect, row.names=F, append=T)
	}

adddbstore<- function(storeid, host, name, uid, pass, port) {
	if(is.na(host)) return(NULL)
	dsn<- 'Mysql Dsn'
	connect<- cbind(storeid[1], dsn[1], host[1], name[1], uid[1], pass[1], port[1])
	colnames(connect)<- c('edb_dbid', 'edb_dsn', 'edb_hostname', 'edb_name', 'edb_user', 'edb_pass', 'edb_port')
	connect[,1]<- as.integer(connect[,1])
	connect[,2:6]<-as.character(connect[,2:6])
	connect[,7]<- as.integer(connect[,7])
	connect<- as.data.frame(connect)
	dbWriteTable(configdb, "etl_dbstore", connect, row.names=F, append=T)
	}

cfgdata<- function(input, output, session) {
	ns<- session$ns
	datadb<- NULL

	observeEvent(input$dbname, {
		cfgdbs<- getdbstores(M$cfg)[1,]
		output$dbdtls<- renderUI(
		fluidPage(
			hr(),
			fluidRow(
				column(2, h4('Connection Details')),
				column(2,textInput(ns('dbhost'), 'Host', value=cfgdbs$edb_hostname)),
				column(2,textInput(ns('dbuser'), 'Userid', value=cfgdbs$edb_user)),
				column(2,passwordInput(ns('dbpass'), 'Password', value=cfgdbs$edb_pass)),
				column(2,textInput(ns('dbport'), 'Port', value=cfgdbs$edb_port)),
				column(2, align='right', bsButton(ns("dbconnect"),"Connect", size='large', style='info'))
				),
			hr(),
			fluidRow(uiOutput(ns('tablist')))
			)
			)
			})
	observeEvent(input$dbconnect, {
		host<- isolate(input$dbhost)
		name<- isolate(input$dbname)
		uid<- isolate(input$dbuser)
		pass<- isolate(input$dbpass)
		port<- as.integer(isolate(input$dbport))
	#	adddbstore(storeid,host,name,uid,pass,port)
		createAlert(session, ns("savedb"), ns("dbsaved"), style='info', title="", content=paste(h5(paste("Connected"))))

		con<- opendb(db=name, host=host, user=uid, password=pass, port=port)
		tabs<- gettabs(con)
		tabs<- as.data.frame(tabs)[,1]
		tabs<- head(tabs, 30)
		tabs<- paste(tabs, collapse=' ')
		output$tablist<- renderUI(
			fluidPage(fluidRow(column(2, h4('Sample Tables')), column(10, tabs)))
			)
		})
#	observeEvent(input$
	}

cfgdataUI<- function(id) {
	ns<- NS(id)
	cfgdbs<- getdbstores(M$cfg)
	print(cfgdbs)

	fluidPage(
	h3('Add New Database Store'),

	fluidRow(
		column(2, h4('Store Name:')), column(2, textInput(ns('storename'), ''))
	),
	fluidRow(
		column(2, h4('Which Database?')), column(2, selectizeInput(ns('dbtype'), '', c('MySQL', 'Oracle')))
	),
	br(),
	fluidRow(
		column(2, h4('Schema Name:')), column(2, selectizeInput(ns("dbname"), "", cfgdbs$edb_name, selected='', multiple=T, options=list(maxitems=1, create=T, placeholder='Select or Add')))
	),
	fluidRow(
		uiOutput(ns('dbdtls'))
		)
	#fluidRow(
		#column(2, h4('Connection Details')), column(10, uiOutput(ns('dbdtls')))
		#),
	#dataTableOutput(ns('tabledump'))
	)
	}
