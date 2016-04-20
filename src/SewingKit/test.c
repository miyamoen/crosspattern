
double abs(double value){
    if (value > 0) {
        return value;
    } else {
        return - value
    }
}
int[] a[10];
for (int i = 0; i < 10; ++i)
{
    a[i] = i;
}

int s = 3;

while(1) {


}


for (int i = 0; i < 10; ++i)
{
    if (a[i] == s)
    {
        break;
    }
}
printf("%d\n番目", i);

int i = 1;
do {
    int j = 1;
    do {
        printf("| %dx%d=%d",i,j,i*j )
        j++;
    } while (j < 10)
    i++;
} while (i < 10)

int main(void) {
    int a0 = 5;
    int a1 = 3;
    int a2 = 9;
    int a3 = 2;
    int a4 = 6;
    double a;
    double res;

    a = (a0 + a1 + a2 + a3 + a4) / 5
    res = ( abs(a0 - a) + abs(a1 - a) + abs(a2 - a) + abs(a3 - a) + abs(a4 - a) ) / 5

    printf("%d\n", res );


}