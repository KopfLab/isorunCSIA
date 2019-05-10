# Ruby guard file (need ruby >2.2 and bundler installed: 'gem install bundler')
# To make sure all the gems are installed, run 'bundle install' once in terminal
# Run lablogger::ll_setup_gui('dev') and fill out the resulting dev/credentials.R file
# Then you can use the Makefile target 'make gui_dev' to start the GUI in development mode
# For browser livereload to work, need the browser extension: http://livereload.com/extensions/#installing-sections
# If the delay is too short for relaunching the app, increase the grace_period

port = 5000

guard 'process', name: 'Shiny', command: ['R', '-e', " \
source('dev/credentials.R'); \
message('INFO: Connection to database... ', appendLF = FALSE); \
pool <- pool::dbPool(drv = RPostgreSQL::PostgreSQL(), host = db_host, dbname = db_name, user = db_user, password = db_pwd); \
message('successful.'); \
shiny::onStop(function() { pool::poolClose(pool) }); \
devtools::load_all('.'); \
isorunCSIA:::turn_debug_on(); \
isorunCSIA::csia_run_gui(instrument_id = instrument_id, pool = pool, port = #{port}, launch = TRUE)"] do
  watch(%r{NAMESPACE})
  watch(%r{R/.+\.R$})
end

guard 'livereload', grace_period: 5 do
  watch(%r{NAMESPACE})
  watch(%r{R/.+\.R$})
end
