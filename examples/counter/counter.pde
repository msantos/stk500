/*
 * Copyright (c) 2012, Michael Santos <michael.santos@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * Neither the name of the author nor the names of its contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
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

    void
setup()
{
    Serial.begin(9600);
    pinMode(LDR, INPUT);
    state.len = htons(sizeof(int));
    state.val = 0;
}

    void
loop()
{
    int i = 0;
    char *p = (char *)&buf;

    if (Serial.available() >= sizeof(buf)) {

        for (i = 0; i < sizeof(buf); i++)  
            *(p+i) = Serial.read();

        switch (buf.type) {
            case 0:  /* Add to counter */
                state.val += buf.val;
                break;

            case 1:  /* Retrieve state */
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

    int
htons(int n)
{
    return ((n & 0xff) << 8) | ((n & 0xff00) >> 8);
}
