int main() {
    return ({ enum t { zero, one, two }; enum t y; sizeof(y); });
}