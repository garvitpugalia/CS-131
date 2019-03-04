import java.util.concurrent.atomic.DoubleAdder;

class BetterSafeState implements State {
    private DoubleAdder[] value;
    private byte maxval;
    
    BetterSafeState(byte[] v) { 
        value = new DoubleAdder[v.length];
        for(int i = 0; i < value.length; i++) {
            value[i] = new DoubleAdder();
            value[i].add(v[i]);
        }
        maxval = 127;
    }

    BetterSafeState(byte[] v, byte m) { 
        value = new DoubleAdder[v.length];
        for(int i = 0; i < value.length; i++) {
            value[i] = new DoubleAdder();
            value[i].add(v[i]);
        }
        maxval = m;
    }

    public int size() { 
        return value.length; 
    }

    public byte[] current() {
        byte[] arr = new byte[value.length];
        for(int i = 0; i < value.length; i++) {
            arr[i] = value[i].byteValue();
        }
        return arr;
    }

    public boolean swap(int i, int j) {
        if (value[i].sum() > 0 && value[j].sum() < maxval) {
            value[i].add(-1);
            value[j].add(1);
            return true;
        }
        return false;
    } 
}
