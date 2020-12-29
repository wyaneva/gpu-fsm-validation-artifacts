/*
 * Copyright 2019 Vanya Yaneva, The University of Edinburgh
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

// This include is in $HOME/partecl-runtime/utils

#include "../utils/utils.h"
#include <stdio.h>
#include <stdlib.h>

#define MAX_TC_LENGTH 1100

int read_transition_pair_sizes(const char *, int[], int);
int read_transition_pairs(const char *, struct transition_pair_visit *[], int);
int should_remove(char[], struct transition_pair_visit *[], const int,
                  const int, int[], int *);

int main(int argc, char **argv) {

  if (argc < 3) {

    printf("Please, provide \n (1) fsm tp filename \n (2) test filename \n (3) print "
           "median (optional).\n IMPORTANT: "
           "Test files should be in descending order based on length. Use "
           "`tac` in bash.\n");
    return 0;
  }

  const char *fsm_tp_filename = argv[1];
  const char *test_filename = argv[2];
  int print_median = 1;

  if (argc >= 4) {
    print_median = atoi(argv[3]);
  }

  // read the fsm transitions
  int num_trans_pairs, start_state, num_states;
  read_fsm_numbers(fsm_tp_filename, &num_trans_pairs, &num_states, NULL,
                   &start_state);

  int num_per_state[num_states];
  for (int i = 0; i < num_states; i++) {
    num_per_state[i] = 0;
  }
  read_transition_pair_sizes(fsm_tp_filename, num_per_state, num_states);

  // read transition pairs
  struct transition_pair_visit *transition_pairs[num_states];
  for (int i = 0; i < num_states; i++) {
    transition_pairs[i] = (struct transition_pair_visit *)malloc(
        sizeof(struct transition_pair_visit) * num_per_state[i]);
  }
  read_transition_pairs(fsm_tp_filename, transition_pairs, num_states);

  // read tests one by one and calculate number to be removed
  FILE *test_file = fopen(test_filename, "r");
  if (test_file == NULL) {
    printf("Could not find file with tests %s.\n", test_filename);
    return -1;
  }

  int num_tests = 0;
  int num_covered_trans = 0;
  int num_removed_tests = 0;
  char line[MAX_TC_LENGTH];
  while (fgets(line, sizeof(line), test_file) != NULL) {

    char *dptr;
    char *prev_dptr;

    // skip test num
    dptr = strchr(line, ' ');

    // get test input
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, ' ');
    char test[MAX_TC_LENGTH];
    copy_word_static(test, &prev_dptr);

    // execute test and mark covered transitions
    if (should_remove(test, transition_pairs, num_trans_pairs, start_state,
                      num_per_state, &num_covered_trans)) {
      num_removed_tests++;
    }

    if (print_median && num_tests % 1000 == 0) {
      printf("finished inputs: %d, removed: %d, covered transpairs: %d\n", num_tests,
             num_removed_tests, num_covered_trans);
    }
    num_tests++;
  }
  fclose(test_file);

  float removed_perc = (num_removed_tests * 100) / num_tests;
  float covered_perc = (num_covered_trans * 100) / num_trans_pairs;
  printf("\ntotal tests\tremoved tests\tremoved perc\tcovered pairs\tcovered "
         "pairs perc\n");
  printf("%d\t\t%d\t\t%.0f%%\t\t%d\t\t%.0f%%\n", num_tests, num_removed_tests,
         removed_perc, num_covered_trans, covered_perc);

  /*
  for(int i = 0; i < num_states; i++) {
    for(int j = 0; j < num_per_state[i]; j++) {

      struct transition_pair_visit trans = transition_pairs[i][j];
      if(!trans.is_visited) {

        //printf("%s %d %d %d\n", trans.transition, trans.start_state,
  trans.next_state, trans.mid_state);
      }
    }
  }
  */
}

int read_transition_pair_sizes(const char *fsm_tp_filename, int num_per_state[],
                               int num_states) {

  FILE *fsm_tp_file = fopen(fsm_tp_filename, "r");
  if (fsm_tp_file == NULL) {
    printf("Could not open file %s.\n", fsm_tp_filename);
    return -1;
  }

  char line[100];
  while (fgets(line, sizeof(line), fsm_tp_file) != NULL) {

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

    // increment the number
    num_per_state[start_state]++;
  }

  fclose(fsm_tp_file);

  return 0;
}

int read_transition_pairs(const char *fsm_tp_filename,
                          struct transition_pair_visit *transition_pairs[],
                          int num_states) {

  int num_per_state[num_states];
  for (int i = 0; i < num_states; i++) {
    num_per_state[i] = 0;
  }

  FILE *fsm_tp_file = fopen(fsm_tp_filename, "r");
  if (fsm_tp_file == NULL) {
    printf("Could not open file %s.\n", fsm_tp_filename);
    return -1;
  }

  char line[100];
  while (fgets(line, sizeof(line), fsm_tp_file) != NULL) {

    char *dptr;
    char *prev_dptr;
    struct transition_pair_visit trans;

    // assign transition
    dptr = strchr(line, ' ');
    char *lineptr = line;
    copy_word_static(trans.transition, &lineptr);

    // assign start state
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, ' ');
    trans.start_state = strtol(prev_dptr, NULL, 10);

    // if the line only has two tokens, skip this line
    if (dptr == NULL) {
      continue;
    }

    // assign next state
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, ' ');
    trans.next_state = strtol(prev_dptr, NULL, 10);

    // assign mid state
    prev_dptr = dptr + 1;
    dptr = strchr(prev_dptr, ' ');
    trans.mid_state = strtol(prev_dptr, NULL, 10);

    trans.is_visited = 0;

    int num = num_per_state[trans.start_state];
    transition_pairs[trans.start_state][num] = trans;
    num_per_state[trans.start_state]++;
  }

  fclose(fsm_tp_file);
  return 0;
}

int are_same_transitions(char *trans1, char *trans2, int trans_length) {

  for (int i = 0; i < trans_length; i++) {
    if (trans1[i] != trans2[i]) {
      return false;
    }
  }
  return true;
}

int should_remove(char test[], struct transition_pair_visit *transition_pairs[],
                  const int num_trans_pairs, const int start_state,
                  int num_per_state[], int *num_covered_trans) {

  int cstate = start_state;
  int found_uncovered = 0;

  char *testptr = test;
  int testlen = strlen(test);

  for (int i = 0; i < testlen; i++) {

    // find transition pairs for the current state
    struct transition_pair_visit *trans_ptr = transition_pairs[cstate];
    int num_trans = num_per_state[cstate];

    int found_trans = 0;
    // go over transition pairs to find the relevant one
    for (int i = 0; i < num_trans; i++) {

      // does transition pair match the test
      int trans_pair_length = strlen(trans_ptr->transition);
      if (!are_same_transitions(trans_ptr->transition, testptr,
                                trans_pair_length)) {
        // transition pair does not match the test, keep looking
        trans_ptr++;
        continue;
      }

      // transition pair is found
      // if not visited, set found uncovered and make visited
      if (!trans_ptr->is_visited) {
        (*num_covered_trans)++;
        found_uncovered = 1;
        trans_ptr->is_visited = 1;
      }

      cstate = trans_ptr->mid_state;
      testptr++;
      found_trans = 1;
      break;
    }

    if (!found_trans) {
      testptr++;
    }
  }

  return !found_uncovered;
}
