import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSetState implements State {
    private AtomicIntegerArray atomic_value;
    private byte maxval;

    //Helper functions to manage conversion between int and byte arrays
    private void createArray(byte[] v) {
	int i;
	int size = v.length;
	int[] arr = new int[size];
	
	for (i = 0; i < size; i++){
	    arr[i] = v[i];
	}
	atomic_value = new AtomicIntegerArray(arr);
    }

    private byte[] convertToByte() {
	int size = atomic_value.length();
	byte[] arr = new byte[size];
	int i;

	for (i = 0; i < size; i++){
	    arr[i] = (byte) atomic_value.get(i);
	}
	return arr;
    }

    //Modified constructors and other methods
    GetNSetState(byte[] v) { createArray(v); maxval = 127; }

    GetNSetState(byte[] v, byte m) { createArray(v); maxval = m; }

    public int size() { return atomic_value.length(); }

    public byte[] current() { return convertToByte(); }

    public boolean swap(int i, int j) {
	if (atomic_value.get(i) <= 0 || atomic_value.get(j) >= maxval) {
	    return false;
	}

	atomic_value.getAndDecrement(i);
	atomic_value.incrementAndGet(j);
	return true;
    }
}
