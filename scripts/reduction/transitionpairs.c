#include "../utils/utils.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, const char **argv) {

  if (argc < 2) {
    printf("Please, provide \n 1) fms file name \n 2) output dir.\n");
    return 0;
  }

  const char *fsm_filename = argv[1];
  const char *output_dir = argv[2];

  int num_states = 0;
  int num_trans = 0;
  read_fsm_numbers(fsm_filename, NULL, &num_states, &num_trans, NULL);

  // counters for the sizes of the lists
  int in_list_size[num_states];
  int out_list_size[num_states];
  for (int i = 0; i < num_states; i++) {
    in_list_size[i] = 0;
    out_list_size[i] = 0;
  }

  // count the transitions
  FILE *fsm_file = fopen(fsm_filename, "r");
  if (fsm_file == NULL) {
    printf("Could not open file %s.\n", fsm_filename);
    return -1;
  }

  char line[100];
  while (fgets(line, sizeof(line), fsm_file) != NULL) {

    char *dptr;
    char *prev_dptr;

    // read transition
    dptr = strchr(line, ' ');

    // read start state
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, ' ');
    int start_state = strtol(prev_dptr, NULL, 10);

    // if the line only has two tokens, skip this line
    if (dptr == NULL) {
      continue;
    }

    // read next state
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, ' ');
    int next_state = strtol(prev_dptr, NULL, 10);

    out_list_size[start_state]++;
    in_list_size[next_state]++;
  }
  fclose(fsm_file);

  // allocate transitions going into a state and out of a state
  struct transition *in_list[num_states];
  struct transition *out_list[num_states];
  for (int i = 0; i < num_states; i++) {
    in_list[i] = (struct transition *)malloc(sizeof(struct transition) *
                                             in_list_size[i]);
    in_list_size[i] = 0;

    out_list[i] = (struct transition *)malloc(sizeof(struct transition) *
                                              out_list_size[i]);
    out_list_size[i] = 0;
  }

  // read transitions
  fsm_file = fopen(fsm_filename, "r");
  if (fsm_file == NULL) {
    printf("Could not open file %s.\n", fsm_filename);
    return -1;
  }

  while (fgets(line, sizeof(line), fsm_file) != NULL) {

    struct transition trans;
    char *dptr;
    char *prev_dptr;

    // assign transition
    dptr = strchr(line, ' ');
    char *lineptr = line;
    copy_word_static(trans.transition, &lineptr);

    // assign start state
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, ' ');
    int start_state = strtol(prev_dptr, NULL, 10);

    // if the line only has two tokens, skip this line
    if (dptr == NULL) {
      continue;
    }

    // assign next state
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, ' ');
    int next_state = strtol(prev_dptr, NULL, 10);

    // assign the output
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, ' ');
    copy_word_static(trans.output, &prev_dptr);

    trans.start_state = start_state;
    trans.next_state = next_state;

    // write the out list
    int size_out_list = out_list_size[start_state];
    out_list[start_state][size_out_list] = trans;
    out_list_size[start_state]++;

    // write the in list
    int size_in_list = in_list_size[next_state];
    in_list[next_state][size_in_list] = trans;
    in_list_size[next_state]++;
  }
  fclose(fsm_file);

  // find out the fsm name
  char *dptr;
  char *prev_dptr = NULL;
  dptr = strchr(fsm_filename, '/');
  while (dptr != NULL) {
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, '/');
  }
  char fsm_name[15];
  if (prev_dptr != NULL) {
    strcpy(fsm_name, prev_dptr);
  } else {
    strcpy(fsm_name, fsm_filename);
  }

  // open a file to write the tansition pairs
  char tp_filename[50];
  sprintf(tp_filename, "%s/transition_pairs/%s", output_dir, fsm_name);

  FILE *tp_file = fopen(tp_filename, "w");
  if (tp_file == NULL) {
    printf("Could not open file %s.\n", tp_filename);
    return -1;
  }

  // find out the number of transition pairs
  int num_trans_pairs = 0;
  for (int i = 0; i < num_states; i++) {
    for (int j = 0; j < in_list_size[i]; j++) {
      for (int k = 0; k < out_list_size[i]; k++) {
        num_trans_pairs++;
      }
    }
  }

  fprintf(tp_file, ".i 2\n");
  fprintf(tp_file, ".o 2\n");
  fprintf(tp_file, ".s %d\n", num_states - 1);
  fprintf(tp_file, ".p %d\n", num_trans);
  fprintf(tp_file, ".t %d\n", num_trans_pairs);

  // construct transition pairs
  for (int i = 0; i < num_states; i++) {
    for (int j = 0; j < in_list_size[i]; j++) {
      for (int k = 0; k < out_list_size[i]; k++) {

        struct transition item_in = in_list[i][j];
        struct transition item_out = out_list[i][k];

        struct transition_pair new_trans;
        // states
        new_trans.start_state = item_in.start_state;
        new_trans.next_state = item_out.next_state;

        // transition
        char *newtransptr = new_trans.transition;
        int in_len = strlen(item_in.transition);
        strcpy(newtransptr, item_in.transition);
        newtransptr += in_len;
        strcpy(newtransptr, item_out.transition); // strcpy should add the
                                                  // terminating '\0' character

        // output
        new_trans.output[0] = item_in.output[0];
        new_trans.output[1] = item_out.output[0];
        new_trans.output[2] = '\0';

        fprintf(tp_file, "%s %d %d %d %s\n", new_trans.transition,
                new_trans.start_state, new_trans.next_state, i, new_trans.output);
      }
    }
  }

  /*
  for (int i = 0; i < num_states; i++) {
    free(in_list[i]);
    free(out_list[i]);
  }
  */

  fclose(tp_file);
}
