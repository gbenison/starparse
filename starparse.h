
typedef void (*ship_item_cb_t)(char* name, char* value);
void starparse(const char* fname, const char* filter, ship_item_cb_t ship_item);
