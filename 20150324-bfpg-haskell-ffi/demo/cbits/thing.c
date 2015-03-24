#include <stdlib.h>
#include <string.h>

#include "thing.h"

int thing_new(int serial, const char *name, thing_t **out) {
	char *my_name = malloc(256 * sizeof (char));

	thing_t *thing = malloc(sizeof (thing_t));
	if (thing != NULL) {
		strncpy(my_name, name, 256);
		thing->serial = serial;
		thing->name = my_name;
		*out = thing;
		return 0;
	}
	else {
		return 1;
	}
}

int thing_get_serial(thing_t *thing) {
	return thing->serial;
}

char *thing_get_name(thing_t *thing) {
	return thing->name;
}

void thing_free(thing_t *thing) {
	free(thing->name);
	free(thing);
}
