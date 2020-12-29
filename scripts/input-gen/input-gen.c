// This include is in $HOME/partecl-runtime/utils
#include "../utils/utils.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_TC_LENGTH 1100

void read_paths(const char *, char[][MAX_TC_LENGTH]);

int calculate_list_sizes(const char *, int *, int *);
int generate_transition_pairs(const char *, int, struct transition *[],
                              struct transition *[], int *, int *);

int main(int argc, const char **argv) {

  if (argc < 5) {
    printf("Please provide: \n 1) an fsm name \n 2) an fsm filename \n 3) a "
           "path filename \n 4) an output dir \n 5) should the tests be sorted "
           "in ascending order.\n");
    return 0;
  }
  const char *fsm_name = argv[1];
  const char *fsm_file = argv[2];
  const char *path_file = argv[3];
  const char *output_dir = argv[4];
  int do_ascending_test_order = atoi(argv[5]);

  int num_states = 0;
  int num_trans = 0;
  int start_state = 0;
  read_fsm_numbers(fsm_file, NULL, &num_states, &num_trans, &start_state);

  // read the paths
  char paths[num_states + 1][MAX_TC_LENGTH];
  for (int i = 0; i < num_states + 1; i++) {
    *paths[i] = '\0';
  }
  read_paths(path_file, paths);

  // open file to write tests
  FILE *tests_f = NULL;
  char tests_filename[50];
  sprintf(tests_filename, "%s/inputs/%s.inputs", output_dir, fsm_name);

  tests_f = fopen(tests_filename, "w");
  if (tests_f == NULL) {
    printf("Error opening file %s!\n", tests_filename);
    printf("Exiting!\n");
    return 0;
  }

  // counters for the sizes of the lists
  int in_list_size[num_states];
  int out_list_size[num_states];
  for (int i = 0; i < num_states; i++) {
    in_list_size[i] = 0;
    out_list_size[i] = 0;
  }

  // calculate the sizes
  calculate_list_sizes(fsm_file, in_list_size, out_list_size);

  // transitions going into a state
  struct transition *in_list[num_states];
  struct transition *out_list[num_states];
  for (int i = 0; i < num_states; i++) {
    in_list[i] = (struct transition *)malloc(sizeof(struct transition) *
                                             in_list_size[i]);
    out_list[i] = (struct transition *)malloc(sizeof(struct transition) *
                                              out_list_size[i]);
  }

  // set to zero before populating the arrays
  for (int i = 0; i < num_states; i++) {
    in_list_size[i] = 0;
    out_list_size[i] = 0;
  }
  generate_transition_pairs(fsm_file, num_states, in_list, out_list,
                            in_list_size, out_list_size);

  int count = 0;
  int maxtestlength = 0;

  /*
  if (do_ascending_test_order) {
    printf("Sorted ascending order!\n");
  } else {
    printf("Not sorted!\n");
  }
  */

  // if not sorted, use Knuth's Algorithm to permutate the tests
  // we assume that the state indices correspond to how far the
  // state is to the start state
  int permutated_indices[num_states];
  for (int i = 0; i < num_states; i++) {
    permutated_indices[i] = i;
  }
  if (!do_ascending_test_order) {
    for (int i = num_states - 1; i >= 0; i--) {
      srand(i);
      int idx = rand() % (i + 1);
      int temp = permutated_indices[idx];
      permutated_indices[idx] = permutated_indices[i];
      permutated_indices[i] = temp;
    }
  }

  for (unsigned i2 = 0; i2 < num_states; i2++) {
    int i = permutated_indices[i2];
    for (int j = 0; j < in_list_size[i]; j++) {
      for (int k = 0; k < out_list_size[i]; k++) {

        // generate the transition pair
        struct transition item_in = in_list[i][j];
        struct transition item_out = out_list[i][k];

        char transition[5];

        char *newtransptr = transition;
        int in_len = strlen(item_in.transition);
        strcpy(newtransptr, item_in.transition);
        newtransptr += in_len;
        strcpy(newtransptr, item_out.transition); // strcpy should add the
                                                  // terminating '\0' character

        // printf("%s %d %d %d\n", transition, item_in.start_state,
        // item_in.next_state, item_out.next_state);
        // generate the test
        int state = item_in.start_state;
        char *path = paths[state];

        int pathlen = strlen(path);
        if (pathlen > 0 || state == start_state) {

          // construct the test
          char *testcase = (char *)malloc(sizeof(char) * MAX_TC_LENGTH);
          sprintf(testcase, "%s%s", path, transition);

          // find if this is the longest test so far
          int length = calculate_test_length(testcase);
          if (length > maxtestlength)
            maxtestlength = length;

          // write the test into file
          count++;
          fprintf(tests_f, "%d %s\n", count, testcase);
          free(testcase);
        }
      }
    }
  }
  fclose(tests_f);
}

void read_paths(const char *filename, char paths[][MAX_TC_LENGTH]) {

  FILE *file = fopen(filename, "r");
  if (file == NULL) {
    printf("Could not find file with paths %s.\n", filename);
    return;
  }

  char line[MAX_TC_LENGTH];
  while (fgets(line, sizeof(line), file) != NULL) {

    char *dptr;
    char *prev_dptr;

    // assign state
    dptr = strchr(line, ' ');
    char *lineptr = line;
    int state = strtol(lineptr, NULL, 10);

    // assign path
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, ' ');
    copy_word_static(paths[state], &prev_dptr);
  }

  fclose(file);
}

int calculate_list_sizes(const char *fsm_filename, int *in_list_size,
                         int *out_list_size) {

  // read transitions
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

    // find start state
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, ' ');
    int start_state = strtol(prev_dptr, NULL, 10);

    // if the line only has two tokens, skip this line
    if (dptr == NULL) {
      continue;
    }

    // find next state
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, ' ');
    int next_state = strtol(prev_dptr, NULL, 10);

    // incremetn counters
    out_list_size[start_state]++;
    in_list_size[next_state]++;
  }
  fclose(fsm_file);

  return 0;
}

int generate_transition_pairs(const char *fsm_filename, int num_states,
                              struct transition *in_list[],
                              struct transition *out_list[], int *in_list_size,
                              int *out_list_size) {

  // read transitions
  FILE *fsm_file = fopen(fsm_filename, "r");
  if (fsm_file == NULL) {
    printf("Could not open file %s.\n", fsm_filename);
    return -1;
  }

  char line[100];
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

    trans.next_state = next_state;
    trans.start_state = start_state;

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
  return 0;
}

