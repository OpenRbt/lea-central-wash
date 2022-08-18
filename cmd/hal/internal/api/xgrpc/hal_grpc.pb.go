// Code generated by protoc-gen-go-grpc. DO NOT EDIT.

package xgrpc

import (
	context "context"
	grpc "google.golang.org/grpc"
	codes "google.golang.org/grpc/codes"
	status "google.golang.org/grpc/status"
	emptypb "google.golang.org/protobuf/types/known/emptypb"
)

// This is a compile-time assertion to ensure that this generated file
// is compatible with the grpc package it is being compiled against.
// Requires gRPC-Go v1.32.0 or later.
const _ = grpc.SupportPackageIsVersion7

// HardwareAccessLayerClient is the client API for HardwareAccessLayer service.
//
// For semantics around ctx use and closing/ending streaming RPCs, please refer to https://pkg.go.dev/google.golang.org/grpc/?tab=doc#ClientConn.NewStream.
type HardwareAccessLayerClient interface {
	RunProgram(ctx context.Context, in *Options, opts ...grpc.CallOption) (*AnswerProgram, error)
	MeasureVolumeMilliliters(ctx context.Context, in *OptionsCommand, opts ...grpc.CallOption) (*AnswerCommand, error)
	Volume(ctx context.Context, in *emptypb.Empty, opts ...grpc.CallOption) (*Answer, error)
}

type hardwareAccessLayerClient struct {
	cc grpc.ClientConnInterface
}

func NewHardwareAccessLayerClient(cc grpc.ClientConnInterface) HardwareAccessLayerClient {
	return &hardwareAccessLayerClient{cc}
}

func (c *hardwareAccessLayerClient) RunProgram(ctx context.Context, in *Options, opts ...grpc.CallOption) (*AnswerProgram, error) {
	out := new(AnswerProgram)
	err := c.cc.Invoke(ctx, "/xgrpc.HardwareAccessLayer/RunProgram", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *hardwareAccessLayerClient) MeasureVolumeMilliliters(ctx context.Context, in *OptionsCommand, opts ...grpc.CallOption) (*AnswerCommand, error) {
	out := new(AnswerCommand)
	err := c.cc.Invoke(ctx, "/xgrpc.HardwareAccessLayer/MeasureVolumeMilliliters", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *hardwareAccessLayerClient) Volume(ctx context.Context, in *emptypb.Empty, opts ...grpc.CallOption) (*Answer, error) {
	out := new(Answer)
	err := c.cc.Invoke(ctx, "/xgrpc.HardwareAccessLayer/Volume", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// HardwareAccessLayerServer is the server API for HardwareAccessLayer service.
// All implementations must embed UnimplementedHardwareAccessLayerServer
// for forward compatibility
type HardwareAccessLayerServer interface {
	RunProgram(context.Context, *Options) (*AnswerProgram, error)
	MeasureVolumeMilliliters(context.Context, *OptionsCommand) (*AnswerCommand, error)
	Volume(context.Context, *emptypb.Empty) (*Answer, error)
	mustEmbedUnimplementedHardwareAccessLayerServer()
}

// UnimplementedHardwareAccessLayerServer must be embedded to have forward compatible implementations.
type UnimplementedHardwareAccessLayerServer struct {
}

func (UnimplementedHardwareAccessLayerServer) RunProgram(context.Context, *Options) (*AnswerProgram, error) {
	return nil, status.Errorf(codes.Unimplemented, "method RunProgram not implemented")
}
func (UnimplementedHardwareAccessLayerServer) MeasureVolumeMilliliters(context.Context, *OptionsCommand) (*AnswerCommand, error) {
	return nil, status.Errorf(codes.Unimplemented, "method MeasureVolumeMilliliters not implemented")
}
func (UnimplementedHardwareAccessLayerServer) Volume(context.Context, *emptypb.Empty) (*Answer, error) {
	return nil, status.Errorf(codes.Unimplemented, "method Volume not implemented")
}
func (UnimplementedHardwareAccessLayerServer) mustEmbedUnimplementedHardwareAccessLayerServer() {}

// UnsafeHardwareAccessLayerServer may be embedded to opt out of forward compatibility for this service.
// Use of this interface is not recommended, as added methods to HardwareAccessLayerServer will
// result in compilation errors.
type UnsafeHardwareAccessLayerServer interface {
	mustEmbedUnimplementedHardwareAccessLayerServer()
}

func RegisterHardwareAccessLayerServer(s grpc.ServiceRegistrar, srv HardwareAccessLayerServer) {
	s.RegisterService(&HardwareAccessLayer_ServiceDesc, srv)
}

func _HardwareAccessLayer_RunProgram_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(Options)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(HardwareAccessLayerServer).RunProgram(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/xgrpc.HardwareAccessLayer/RunProgram",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(HardwareAccessLayerServer).RunProgram(ctx, req.(*Options))
	}
	return interceptor(ctx, in, info, handler)
}

func _HardwareAccessLayer_MeasureVolumeMilliliters_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(OptionsCommand)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(HardwareAccessLayerServer).MeasureVolumeMilliliters(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/xgrpc.HardwareAccessLayer/MeasureVolumeMilliliters",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(HardwareAccessLayerServer).MeasureVolumeMilliliters(ctx, req.(*OptionsCommand))
	}
	return interceptor(ctx, in, info, handler)
}

func _HardwareAccessLayer_Volume_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(emptypb.Empty)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(HardwareAccessLayerServer).Volume(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/xgrpc.HardwareAccessLayer/Volume",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(HardwareAccessLayerServer).Volume(ctx, req.(*emptypb.Empty))
	}
	return interceptor(ctx, in, info, handler)
}

// HardwareAccessLayer_ServiceDesc is the grpc.ServiceDesc for HardwareAccessLayer service.
// It's only intended for direct use with grpc.RegisterService,
// and not to be introspected or modified (even as a copy)
var HardwareAccessLayer_ServiceDesc = grpc.ServiceDesc{
	ServiceName: "xgrpc.HardwareAccessLayer",
	HandlerType: (*HardwareAccessLayerServer)(nil),
	Methods: []grpc.MethodDesc{
		{
			MethodName: "RunProgram",
			Handler:    _HardwareAccessLayer_RunProgram_Handler,
		},
		{
			MethodName: "MeasureVolumeMilliliters",
			Handler:    _HardwareAccessLayer_MeasureVolumeMilliliters_Handler,
		},
		{
			MethodName: "Volume",
			Handler:    _HardwareAccessLayer_Volume_Handler,
		},
	},
	Streams:  []grpc.StreamDesc{},
	Metadata: "hal.proto",
}
