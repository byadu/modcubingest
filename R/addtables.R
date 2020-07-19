doadddata<- function(est_storeid, est_storename, est_storetype, est_dirdsn, est_dbdirid) {
	cat('addstore', est_storeid, est_storename, est_storetype, est_dirdsn, est_dbdirid, '\n') 
#	dbWriteTable(configdb, "etl_dbstore", connect, row.names=F, append=T)
	}

addtables<- function(input, output, session) {
	ns<- session$ns
	datadb<- NULL

		ts<- tabstats(M$cfg, input$storename)
		id<- as.integer(rownames(ts))
        rfact<- paste0('<input type="radio" name="main-', id, '"', ifelse(ts$Size=='large', ' checked', ''), ' value=', '1 ></input>')
        rdim<- paste0('<input type="radio" name="main-', id, '"', ifelse(ts$Size!='large', ' checked', ''), ' value=', '2 ></input>')
		ts<- cbind(ts, Fact=rfact)
		ts<- cbind(ts, Dimension=rdim)
		cbox<- character(nrow(ts))
		for(i in 1:nrow(ts))
			cbox[i]<- as.character(checkboxInput(paste0(ns("cbox"), i), NULL, FALSE))
		ts<- cbind(ts, Add=cbox)

	output$tabstat<- DT::renderDataTable(server=F,
	  datatable(ts, selection='none', escape = FALSE,
	  	options=list(pageLength=5, autoWidth=T, columnDefs = list(list(width = '40%', targets=1), list(width='20%', targets=2), list(width='10%', targets=3),list(width='10%', targets=4),list(width='10%', targets=5),list(width='10%', targets=6)),
        	preDrawCallback = JS('function() { Shiny.unbindAll(this.api().table().node()); }'), 
      		drawCallback = JS('function() { Shiny.bindAll(this.api().table().node()); } ')
  			), 
        callback=JS("table.rows().every(function(i, tab, row) { var $this = $(this.node()); $this.attr('id', 'main-'.concat(this.data()[0])); $this.addClass('shiny-input-radiogroup'); }); Shiny.unbindAll(table.table().node()); Shiny.bindAll(table.table().node());")
		)
		 %>%
		formatStyle('Rows', background=styleColorBar(ts$Rows, 'steelblue'), backgroundSize = '95% 70%', backgroundRepeat = 'no-repeat', backgroundPosition = 'right') %>%
		formatStyle('Size', target='row', backgroundColor=styleEqual(levels(ts$Size), c('white', 'lightblue', 'lightgreen')))
		)
		

		dims<- list()
		facts<- list()
		dsel<- NULL

	  	output$sel<- renderDataTable({
			d<- 0
			f<- 0
		for(i in 1:nrow(ts)) {
			radio<- as.character(i)
			cbox<- paste0("cbox", i)
			add<- input[[cbox]]
			if(!is.null(add) && add==T) {
				cat("row", i, "radio", input[[radio]], "..\n")
				if(input[[radio]]==2) {
					d<- d+1
					dims[[d]]<- as.character(ts[i,1])
					}
				else {
					f<- f+1
					facts[[f]]<- as.character(ts[i,1])
					}
					}
			}
			if(f>0) {
				dsel<<- data.frame(Table=unlist(facts), Type='Fact')
				if(d>0) {
					dlist<- data.frame(Table=unlist(dims), Type='Dims')
					dsel<<- rbind(dsel, dlist)
					}
				}
			else if(d>0)
				dsel<<- data.frame(Table=unlist(dims), Type='Dims')
			datatable(dsel,selection='none', options=list(dom='tp',pageLength=5))
			})

	observeEvent(input$add, {
		createAlert(session, ns("added"), ns("addedmsg"), style='default', title="", content=paste(h5('Added:'), paste(dsel$Table, collapse=', ')))
		}
		)

	observeEvent(input$storename, ignoreInit=T, ignoreNULL=T, {
		output$storedtls<- renderUI(
			fluidPage(
			fluidRow(
			column(8, dataTableOutput(ns('tabstat'))),
			column(4, 
				h4('Tables Selected:'),
				dataTableOutput(ns('sel')),
				bsButton(ns('add'), 'Add Tables', size='large', style='primary'),
				bsAlert(ns('added'))
				)
			)
			)
		)
		})

	observeEvent(input$dbconnect, {
		host<- isolate(input$dbhost)
		name<- isolate(input$dbname)
		uid<- isolate(input$dbuser)
		pass<- isolate(input$dbpass)
		port<- isolate(input$dbport)
	#	adddbstore(storeid,host,name,uid,pass,port)
		createAlert(session, ns("savedb"), ns("dbsaved"), style='info', title="", content=paste(h5(paste("Connected"))))
d

		datadb<<-dbConnect(MySQL(), user=uid,password=pass,dbname=name, host=host, port=as.integer(port))
		tabs<- getalltabs(datadb, name)
		tabs<- as.vector(tabs[,1])
		tabs<- head(tabs, 50)
		tabs<- paste(tabs, collapse=' ')

		output$tablist<- renderUI(
			fluidPage(fluidRow(column(2, h4('Sample Tables')), column(10, tabs)))
			)
		})
	}

addtablesUI<- function(id) {
	ns<- NS(id)
	dbstores<- getdbstores(M$cfg)
	stores<- dbstores$est_storename
print(stores)
	
	a<-htmlTemplate('templs/addtables.html', storename=selectInput(ns('storename'), '', stores), storedtls=uiOutput(ns('storedtls')))  
	a
	}

