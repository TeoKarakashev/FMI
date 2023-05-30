int n;
int shots[100002];
int res = 0;

void solve()
{
    int posAlex = n - 1;
    int posMila = 0;
    long long alexShots = shots[n - 1];
    long long milaShots = shots[0];

    while (posMila < posAlex)
    {
        if (alexShots == milaShots)
        {
            res = (n - posAlex) + (posMila + 1);

            posMila++;
            posAlex--;

            alexShots += shots[posAlex];
            milaShots += shots[posMila];
        }
        else
            if (alexShots < milaShots)
            {
                posAlex--;
                alexShots += shots[posAlex];
            }
            else
            {
                posMila++;
                milaShots += shots[posMila];
            }
    }

}
int main()
{
    ios_base::sync_with_stdio(0);
    cin.tie(0);
    cout.tie(0);

    cin >> n;
    for (int i = 0; i < n; i++)
        cin >> shots[i];
    
    solve();
    cout << res << endl;
    return 0;
}