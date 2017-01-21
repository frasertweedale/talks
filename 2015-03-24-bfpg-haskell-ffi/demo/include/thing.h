typedef struct thing {
	int serial;
	char *name;
} thing_t;

int thing_get_serial(thing_t *thing);

char *thing_get_name(thing_t *thing);

int thing_new(int serial, const char *name, thing_t **p);

void thing_free(thing_t *thing);
