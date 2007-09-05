
typedef void (*ship_item_cb_t)(char* name, char* value);
typedef void (*starparse_error_handler_t)(char* msg);
void starparse(const char* fname,
	       const char* filter,
	       ship_item_cb_t ship_item,
	       starparse_error_handler_t error_handler);
