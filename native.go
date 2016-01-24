package sproc

type NativeFunc func(args []*Value, in, out *StreamSlot)

func NativeCat(args []*Value, in, out *StreamSlot) {
	for {
		v, ok := in.Recv()
		if !ok {
			return
		}

		out.Send(v)
	}
}
