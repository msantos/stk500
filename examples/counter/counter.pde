int LDR = 2;
int inb = 0;

/* sizeof(int) = 2, struct is not padded */
struct {
  int type;
  int val;
} buf;

struct {
  int len;
  int val;
} state;

unsigned int offset = 0;

void setup() {
  Serial.begin(9600);
  pinMode(LDR, INPUT);
  state.len = htons(sizeof(int));
  state.val = 0;
}

void loop() {
  int i = 0;
  char *p = (char *)&buf;

  if (Serial.available() >= sizeof(buf)) {

    for (i = 0; i < sizeof(buf); i++)
      *(p + i) = Serial.read();

    switch (buf.type) {
    case 0: /* Add to counter */
      state.val += buf.val;
      break;

    case 1: /* Retrieve state */
      /* Convert to big endian */
      Serial.write((const uint8_t *)&state, sizeof(state));
      break;

    case 2: /* Initialize state */
      state.val = buf.val;
      break;

    default:
      break;
    }
  }
}

int htons(int n) { return ((n & 0xff) << 8) | ((n & 0xff00) >> 8); }
