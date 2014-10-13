
unsigned int* encode(int size, unsigned int* a) {

  unsigned int* b = new unsigned int[size / 16];
  for (int i = 0; i < size / 16; ++i) {
    b[i] = 0;
  }	
 
  for (int i = 0; i < size; ++i)
    for (int j = 0; j < size; ++j)
      b[(i + j) / 32] ^= ( (a[i / 32] >> (i % 32)) &
		       (a[j / 32 + size / 32] >> (j % 32)) & 1 ) << ((i + j) % 32);

  return b;
}

