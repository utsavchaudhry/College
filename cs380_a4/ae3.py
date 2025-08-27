import sys
import torch.nn as nn
from autoencoder import _Autoencoder
from data import Data
from model import Model
from ae1 import AE1
from ae2 import AE2

class AE3(_Autoencoder):
    def __init__(self, path):
        super().__init__(path)
        n_kernels = 64
        
        self.encoder = Model(
            input_shape=(self.BATCH_SIZE, n_kernels, 16, 16),
            layers=[
                nn.Conv2d(n_kernels, n_kernels, kernel_size=3, padding=1),
                nn.ReLU(),
                nn.MaxPool2d(kernel_size=2)
            ]
        )
        
        self.decoder = Model(
            input_shape=(self.BATCH_SIZE, n_kernels, 8, 8),
            layers=[
                nn.ConvTranspose2d(n_kernels, n_kernels, kernel_size=3, stride=2, padding=1, output_padding=1),
                nn.ReLU()
            ]
        )
        
        self.model = Model(
            input_shape=self.encoder.input_shape,
            layers=[self.encoder, self.decoder]
        )

if __name__ == '__main__':
    epochs = int(sys.argv[1]) if len(sys.argv) > 1 else None
    data = Data.load('data', image_size=64)
    data.shuffle()
    
    ae1 = AE1('models/ae1.pt')
    ae2 = AE2('models/ae2.pt')
    print(f'\nLoading AE1 from {ae1.path}...')
    ae1.load()
    print(f'Loading AE2 from {ae2.path}...')
    ae2.load()
    
    data_encoded1 = ae1.encode(data)
    data_encoded2 = ae2.encode(data_encoded1)   # Obtain AE2's encoder output as training data for AE3
    ae3 = AE3('models/ae3.pt')
    ae3.print()
    if not epochs:
        print(f'\nLoading {ae3.path}...')
        ae3.load()
    else:
        # Train AE3 on AE2-encoded data
        print(f'\nTraining AE3 for {epochs} epochs...')
        ae3.train(epochs, data_encoded2)
        print(f'\nSaving model to {ae3.path}...')
        ae3.save()
    
    print(f'\nGenerating reconstructed codes with AE3...')
    reconstructed = ae3.generate(data_encoded2)
    
    images_reconstructed = ae1.decode(ae2.decode(reconstructed))
    
    data.display(16)
    images_reconstructed.display(16)
