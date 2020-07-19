xf<- reactiveValues(applied=0)

sheets<- c()
sheetdata<- list()

xldispUI<- function(id, xlfile) {
	ns<- NS(id)

	sheets<<- excel_sheets(xlfile)
	xltabs<- list()
	for(i in 1:length(sheets)) {
		xltabs[[i]]<- tabPanel(title=sheets[i], box(title=NULL, width=12, DTOutput(ns(paste0('sheet', i)))))
		}
	do.call(tabsetPanel,xltabs)
	}

xldisp<- function(input, output, session, xlfile, skiprows) {
	sheets<<- excel_sheets(xlfile)
	for(i in 1:length(sheets)) {
		df<- as.data.frame(read_excel(xlfile, sheet=i, skip=skiprows))
		colnames(df)<- gsub(" ", "_", colnames(df))
		sheetdata[[i]]<<- df
		local({
			my_i<- i
			sheetid<- paste0('sheet', my_i)
			output[[sheetid]]<- renderDT({
	xf$applied
				datatable(sheetdata[[my_i]], class='compact', options=list(dom='tp', autowidth=F, pageLength=10, scrollX=T))
				})
			})
		}
	}

addxl<- function(input, output, session) {
	ns<- session$ns

	writetabs<- function(xlfile) {
		for(i in 1:length(sheets)) {
			write.csv(file=sheets[i], sheetdata[[i]])
			dbWriteTable(D$mydata, sheets[i], sheetdata[[i]], row.names=F, overwrite=T)
			tl<- data.frame(cbind(sheets[i][1], 2))
			colnames(tl)<- c('tl_tab', 'tl_type')
			res<- dbWriteTable(D$mydata, 'table_list', tl, row.names=F, append=T)
			cat('table list write' , res, '\n')
			}
		closeAlert(session, ns('doneup'))
		createAlert(session, style='success', ns('updone'), ns('doneup'), title=NULL, content=paste(length(sheets), "Tables Written"))
		}

	observeEvent(input$xlup, {
		writetabs(isolate(input$filename$datapath))
		})

	output$xl2db<- renderUI({
		req(input$filename)
		fluidPage(
			fluidRow(
				bsButton(block=T, ns('xlup'), 'Upload this Excel to Database', style='primary')
				),
			fluidRow(
				bsAlert(ns('updone'))
				)
			)
			})

	output$xldtls<- renderUI({
		req(input$filename)
		callModule(xldisp, 'addxl', input$filename$datapath, input$skiprows)
		xldispUI(ns('addxl'), input$filename$datapath)
		})

	observeEvent(input$xapply, ignoreInit=T, ignoreNULL=T, {
		field<- input$field
		rx<- input$rscript
		df<- sheetdata[[1]]
		if(grepl(' ', field))
			gfield<- paste0('`', field, '`')
		else
			gfield<- field
		xform<- gsub(gfield, paste0('df$',gfield), rx)
		print(xform)
#		xform<- paste0("df[",rx, ",]")
		print(head(df,1))
		xval<- eval(parse(text=xform))
		print(head(xval))
		#df[[field]]<- eval(parse(
		df[[field]]<- xval
		print(head(df))
		sheetdata[[1]]<<- df
		print(sheets[1])
		xf$applied=1
		})
	output$xform<- renderUI({
		req(input$filename)
		box(title='Transform Field', width=12,
		fluidPage(
			column(3, selectInput(ns('field'), label=NULL, colnames(sheetdata[[1]]))),
			column(7, textAreaInput(height='100%',width='600px', ns('rscript'), label=NULL, placeholder='Enter R Transform Script for this field')),
			column(2, bsButton(block=T, ns('xapply'), 'Apply Transform', style='primary'))
			)
		)
		})
	}

addxlUI<- function(id) {
	ns<- NS(id)

	fluidPage(
	fluidRow(
		column(9, box(title=NULL, width=12,
			column(9, fileInput(placeholder=".xls or .xlsx file", ns('filename'), width='100%', label="Add New Excel", accept=c('.xls', '.xlsx'))),
			column(3, numericInput(ns('skiprows'), label='Rows to skip', value=0))
			)),
		column(3, uiOutput(ns('xl2db')))
		),
	br(),
	fluidRow(
		uiOutput(ns('xform')),
		uiOutput(ns('xldtls'))
		)
	)
	}
